#!/usr/bin/python3
import subprocess
import re
import os
import sys
import string
import networkx as nx

from collections import defaultdict

CLASS_NAME_STR = "public class"
MY_GLOBAL_HELPER_CLASS = "UNKNOWN"
DEFAULT_STRUCT = "struct " + MY_GLOBAL_HELPER_CLASS + "*"
SEPARATOR_STRING = "// Code above this line has been added to remove errors"
PUBLIC_CLASS_STRING = "// This class was public"
C_BASIC_DATA_TYPE = ["int", "short", "long", "char", "float", "double"]

DEFAULT_TYPE = "void *"

DUMMY_RETURN_TYPE = MY_GLOBAL_HELPER_CLASS

LIST_OF_INTERNAL_EXCEPTIONS = ["UncheckedIOException", "ArithmeticException", "ArrayIndexOutOfBoundsException",
                               "ArrayStoreException", "ClassCastException", "IllegalArgumentException",
                               "IllegalMonitorStateException", "IllegalStateException", "IllegalThreadStateException",
                               "IndexOutOfBoundsException", "NegativeArraySizeException", "NullPointerException",
                               "NumberFormatException", "SecurityException", "UnsupportedOperationException"]
NATIVE_ARRAY_VARIABLES = ["length"]
RESERVED_KEYWORDS = ["super"]
VALID_VARIABLE_CHARS = string.ascii_letters + string.digits + "_"
VALID_CHARS = VALID_VARIABLE_CHARS + "$.()[]"
IDENTIFIERS_FOR_FUNCTION = ["public", "private", "protected"]


def compile_file_and_get_output(file_path):
    tmp_output_path = file_path[:file_path.rfind('/')] + '/tmp/' + file_path[file_path.rfind('/') + 1:]
    # print(tmp_output_path)
    cmd = ["clang", file_path, "-emit-llvm", "-S", "-c", "-o", tmp_output_path]
    # a = os.system(cmd)
    return subprocess.run(cmd, stderr=subprocess.PIPE).stderr.decode('utf-8')
    # return subprocess.run(['javac', file_path, '-d', '/tmp/'], stderr=subprocess.PIPE).stderr.decode('utf-8')


def get_file_content(file_path):
    content = None
    if not os.path.exists(file_path):
        print("file not exists:", file_path)
    with open(file_path, 'r') as f:
        content = f.read()
    return content


def remove_file_if_exists(file_path):
    try:
        os.remove(file_path)
    except OSError:
        pass


def duplicate_file(original_file, new_file):
    if (new_file.rfind('/') != -1):
        dir_path = new_file[:new_file.rfind('/')]
        subprocess.run(['mkdir', '-p', dir_path], stderr=subprocess.PIPE).stderr.decode('utf-8')
    subprocess.run(["cp", original_file, new_file], stderr=subprocess.PIPE).stderr.decode('utf-8')


def write_file_content(file_path, file_content):
    if (file_path.rfind('/') != -1):
        dir_path = file_path[:file_path.rfind('/')]
        subprocess.run(['mkdir', '-p', dir_path], stderr=subprocess.PIPE).stderr.decode('utf-8')
    with open(file_path, 'w') as f:
        f.write(file_content)


def get_full_path_of_files_in_folder(folder_path):
    l_docs = []
    paths = os.listdir(folder_path)
    for i in range(len(paths)):
        paths[i] = folder_path + paths[i]
        if (os.path.isdir(paths[i])):
            l_docs.extend(get_full_path_of_files_in_folder(paths[i] + "/"))
        else:
            l_docs.append(paths[i])
    return l_docs


""" Functions to edit code """


def get_l_of_params(s):
    if (s == ""):
        return []
    s = s.replace("<null>", "Object")
    s = re.sub('Class<[^>]*>', 'Class', s)
    l_p = s.split(",")
    to_ret = []
    pending = []
    for i in range(len(l_p)):
        if ("<" in l_p[i] or ">" in l_p[i]):
            pending.append(l_p[i])
        else:
            if (pending != []):
                to_ret.append(",".join(pending))
                pending = []
            to_ret.append(l_p[i])

    if (pending != []):
        to_ret.append(",".join(pending))
    return to_ret


def make_dummy_body(return_type):
    if return_type == "int" or return_type == "byte" or return_type == "long":
        return "{return 1;}"
    elif return_type == "boolean":
        return "{return True;}"
    elif return_type == "float" or return_type == "double":
        return "{return 1.0;}"
    elif return_type == "char":
        return "{return 'c';}"
    else:
        return "{return null;}"


def make_dummy_method(method_sig):
    ret_type = method_sig[0]

    method_name = method_sig[1].split("(")[0]
    method_param_types = get_l_of_params(method_sig[1].split("(")[1][:-1])
    method_args = []
    for ctr in range(len(method_param_types)):
        method_args.append(method_param_types[ctr] + " o" + str(ctr))

    method_args = "(" + ", ".join(method_args) + ")"
    method_definition = '\tpublic ' + ret_type + ' ' + method_name + method_args + make_dummy_body(ret_type) + '\n'

    return method_definition


def make_dummy_variable(var):
    if "#ptr" in var[0]:#function ptr
        ret_type = re.sub("#ptr", "", var[0])
        return ret_type + " (*" + var[1] + ")();\n"
    if (type(var) != type('') and len(var) == 2):
        if "[10]" in var[0]:
            # return '\t' + var[0].replace("[10]", "") + ' ' + var[1] + "[10]" + ";\n"
            return '\t' + var[0].replace("[10]", "*") + ' ' + var[1] + ";\n"
        return '\t' + var[0] + ' ' + var[1] + ";\n"
    else:
        return '\t' + DUMMY_RETURN_TYPE + ' ' + var + ";\n"


def make_dummy_constructor(class_name, cons_arg):
    cons_param_types = get_l_of_params(cons_arg)
    cons_params = []
    for ctr in range(len(cons_param_types)):
        cons_params.append(cons_param_types[ctr] + " o" + str(ctr))
    cons_params = "(" + ", ".join(cons_params) + ")"
    return '\t' + class_name + cons_params + '{}\n'


def make_dummy_definition(ident):
    global identifiers

    ident_value = identifiers[ident]

    # return "#define " + ident + " " + ident_type + " " + ident_value + "\n"
    return "#define " + ident + " " + ident_value + "\n"


def make_dummy_global_vars(var):
    global allocate_value
    #global global_vars
    var_type = global_vars[var]
    if "[10]" in global_vars[var]:
        var_type = var_type.replace("[10]", "")
        if "#constant" in var_type:
            var_type = var_type.replace("#constant", "")
            # return var_type + " " + var + "[10]" + ";\n"
            return var_type + "* " + var + ";\n"
        # return var_type + " " + var + "[10]" + ";\n"
        return var_type + "* " + var + ";\n"
    if "#constant" in var_type:
        var_type = var_type.replace("#constant", "")
        allocate_value += 1
        return "const " + var_type + " " + var + " = " + str(allocate_value) + ";\n"
    return global_vars[var] + " " + var + ";\n"


def make_dummy_class(class_name, list_of_variables, list_of_method_signatures, list_of_constructor_args):
    global identifiers

    variable_definitions = ""
    method_definitions = ""

    identifier_definitions = ""

    for var in list_of_variables:
        variable_definitions += make_dummy_variable(var)

    for cons_arg in list_of_constructor_args:
        method_definitions += make_dummy_constructor(class_name, cons_arg)

    for method_sig in list_of_method_signatures:
        method_definitions += make_dummy_method(method_sig)

    method_definitions = method_definitions.strip()
    variable_definitions = variable_definitions.strip()
    if class_name not in ptr_tag:
        class_code = "typedef struct " + class_name + " {\n\t" + variable_definitions + \
                 "\n\t" + method_definitions + "\n}" + class_name + ";\n"
    else:
        class_code = "struct " + class_name+"_t" + " {\n\t" + variable_definitions + \
                     "\n\t" + method_definitions + "\n}" + ";\n"
        class_code += "typedef struct " + class_name + "_t*" + " " + class_name + ";\n"
    return class_code


def make_dummy_exception(class_name, list_of_constructor_args, list_of_method_signatures):
    method_definitions = "\tpublic " + class_name + "(String errorMessage) { super(errorMessage); }\n"
    for cons_arg in list_of_constructor_args:
        method_definitions += make_dummy_constructor(class_name, cons_arg)

    for method_sig in list_of_method_signatures:
        method_definitions += make_dummy_method(method_sig)

    class_code = "class " + class_name + " extends Exception{\n" + method_definitions + "}\n"
    return class_code


def get_existing_class_names():
    global l_code

    contenders = []
    for line_no, line in enumerate(l_code):
        if (SEPARATOR_STRING in line):
            break
        if (line.startswith("public class ")):
            contenders.append((line_no, line.split(" ")[2]))
        if (line.startswith("final class ")):
            contenders.append((line_no, line.split(" ")[2]))
        if (line.startswith("class ")):
            class_name = line.split(" ")[1]
            if "{" in class_name:
                class_name = class_name[:class_name.index("{")]
            contenders.append((line_no, class_name))

        if (line.strip().startswith("@")):
            l_code[line_no] = "//" + l_code[line_no]
    return contenders


def get_code_for_new_class(class_name):
    l_var = [i[1] if len(i) == 2 else i[1:] for i in d_classes_to_add[class_name] if i[0] == "VAR"]
    l_method = [i[1] if len(i) == 2 else i[1:] for i in d_classes_to_add[class_name] if i[0] == "METHOD"]
    l_exception = [i[1] for i in d_classes_to_add[class_name] if i[0] == "EXCEPTION"]
    l_constructor = [i[1] for i in d_classes_to_add[class_name] if i[0] == "CONSTRUCTOR"]

    if (len(l_exception) != 0):
        class_code = make_dummy_exception(class_name, l_constructor, l_method)
    else:
        class_code = make_dummy_class(class_name, l_var, l_method, l_constructor)

    return class_code.split("\n")


def get_code_for_existing_class(class_name):
    l_var = [i[1] if len(i) == 2 else i[1:] for i in d_classes_to_add[class_name] if i[0] == "VAR"]
    l_method = [i[1] if len(i) == 2 else i[1:] for i in d_classes_to_add[class_name] if i[0] == "METHOD"]

    # print(l_var)
    method_definitions = ""
    variable_definitions = ""
    for method_sig in l_method:
        method_definitions += make_dummy_method(method_sig)
    for variable_sig in l_var:
        variable_definitions += make_dummy_variable(variable_sig)

    class_code = variable_definitions + method_definitions
    return class_code


def add_members_of_existing_existing_class():
    global l_code
    global d_classes_to_add
    global existing_class_names

    existing_class_names = get_existing_class_names()

    for line_no, class_name in existing_class_names:
        # print(class_name)
        class_code = get_code_for_existing_class(class_name).strip().split("\n")
        if (class_code != ['']):
            l_code = l_code[:line_no + 1] + class_code + l_code[line_no + 1:]
        # reset dictionary for existing classes
        d_classes_to_add[class_name] = set()


def copy_a_graph(ori_graph):
    new_graph = nx.DiGraph()

    for pre, nxt in ori_graph.edges():
        new_graph.add_edge(pre, nxt)

    return new_graph


def get_new_code_to_add():
    global identifiers
    #global i

    new_code_to_add = []

    identifier_definitions = ""

    # for ident in identifiers.keys():
    #    identifier_definitions += make_dummy_definition(ident)

    # if identifier_definitions:
    #    new_code_to_add += identifier_definitions.split("\n")
    for define_line in define_lines:
        new_code_to_add += [define_line + "\n"]

    queue = []
    searched = []
    tmp_order_graph = copy_a_graph(struct_order_graph)

    for struct_name in tmp_order_graph.nodes():
        if tmp_order_graph.in_degree(struct_name) == 0:
            queue.append(struct_name)
            searched.append(struct_name)

    # print(queue)
    while queue:
        struct_name = queue.pop(0)
        if struct_name in d_classes_to_add:
            new_code_to_add += get_code_for_new_class(struct_name)
        for nxt_name in tmp_order_graph.neighbors(struct_name):
            if nxt_name not in searched and tmp_order_graph.in_degree(nxt_name) == 1:
                queue.append(nxt_name)
                searched.append(nxt_name)
        tmp_order_graph.remove_node(struct_name)

    for struct_name in d_classes_to_add.keys():
        if struct_name not in searched:
            new_code_to_add += get_code_for_new_class(struct_name)
        # print(new_code_to_add)

    for var in global_vars.keys():
        new_code_to_add += [make_dummy_global_vars(var)]

    return new_code_to_add


def compress_code(source_code):
    lines = source_code.split("\n")
    new_lines = []
    line_id = len(lines) - 1
    pre_line = ""
    while (line_id >= 0):
        current_line = lines[line_id]
        if current_line.strip().startswith("."):
            pre_line = current_line.strip() + pre_line
        else:
            current_line = current_line.rstrip() + pre_line
            new_lines.insert(0, current_line)
            pre_line = ""
        line_id -= 1
    new_source_code = ""
    for line in new_lines:
        new_source_code += line + "\n"
    # print(new_source_code)
    return new_source_code


def get_new_code_line_after_add_data_type(pointer, valid_chars, target_type, code_line, direction):
    target_type = "(" + target_type + ")" + "(Object)"
    if direction == "right":
        ptr = len(pointer) - 1

        while (code_line[ptr] not in valid_chars):
            ptr = ptr + 1
            if (ptr > len(code_line)):
                return code_line
        return code_line[:ptr] + target_type + code_line[ptr:]

    # direction = "left"
    ptr = len(pointer) - 2
    num_brackets = 0
    num_sq_brackets = 0

    # print(code_line, code_line[ptr + 1])
    if (code_line[:ptr + 1].strip() in ["switch"]):
        ptr += 1

    if (code_line[ptr + 1] not in valid_chars):
        # bitPointer = 8 + i;
        while (code_line[ptr] == " "):
            ptr -= 1
            if (ptr < 0):
                return code_line

    while (num_sq_brackets != 0 or num_brackets != 0 or code_line[ptr] in valid_chars):
        if (code_line[ptr] == "]"):
            num_sq_brackets += 1
        elif (code_line[ptr] == "[" and num_sq_brackets != 0):
            num_sq_brackets -= 1

        elif (code_line[ptr] == ")"):
            num_brackets += 1
        elif (code_line[ptr] == "(" and num_brackets != 0):
            num_brackets -= 1

        elif (code_line[ptr] in "[(" and num_brackets == 0 and num_sq_brackets == 0):
            break
        ptr -= 1

        if (ptr < 0):
            return code_line

    if (code_line[ptr] != " "):
        ptr += 1

    if (code_line[ptr:].strip().startswith("return")):
        while (code_line[ptr] == " "):
            ptr += 1
        ptr += len("return ")

    if (code_line[ptr - 3:ptr] == "new"):
        # code_line = code_line[:ptr-3] + "(" + code_line[ptr-3:-1] + ")" + code_line[-1]
        ptr -= 3

    return code_line[:ptr] + target_type + code_line[ptr:]


def compare_args_by_index(sig_src, sig_dst):
    l_src = sig_src[1:-1].split(",")
    l_dst = sig_dst[1:-1].split(",")

    for i in range(len(l_src)):
        if (l_src[i] != l_dst[i]):
            return i, l_dst[i]


def get_new_code_line_after_add_suitable_method_signature(pointer, method_desc, code_line, possible_matches):
    method_name = method_desc.split("(")[0].strip()
    method_args = method_desc.strip()[len(method_name):]

    # always apply the match - argument mismatch; ... cannot be converted to ...
    match_to_apply = ""
    for i in possible_matches:
        if (i[1].startswith("(argument mismatch;")):
            match_to_apply = i[0][i[0].find("("): i[0].find(")") + 1]
            break

    if (match_to_apply == ""):  # no match found - make new method/constructor otherwise
        class_name = possible_matches[0][0].split(" ")[1].split(".")[0]
        if (possible_matches[0][0].startswith("method")):
            reverse_method_to_class_mapping[method_desc] = class_name
            d_classes_to_add[class_name].add(("METHOD", DUMMY_RETURN_TYPE, method_desc))
        else:
            d_classes_to_add[class_name].add(("CONSTRUCTOR", method_desc.split("(")[1][:-1]))
        return code_line

    index_to_change, target_type = compare_args_by_index(method_args, match_to_apply)
    target_type = "(" + target_type + ")" + "(Object)"

    ptr = len(pointer) - 2
    while (not (
            code_line[ptr:].startswith(method_name) or code_line[ptr:].startswith("this") or code_line[ptr:].startswith(
        "super"))):
        ptr += 1
        if (ptr > len(code_line)):
            return code_line

    num_brackets = num_comma = 0

    while (not (num_brackets == 1 and num_comma == index_to_change)):
        if (code_line[ptr] == ")"):
            num_brackets -= 1
        elif (code_line[ptr] == "("):
            num_brackets += 1
        if (num_brackets == 1 and code_line[ptr] == ","):
            num_comma += 1
        ptr += 1

        if (ptr > len(code_line)):
            return code_line

    code_line = code_line[:ptr] + target_type + code_line[ptr:]
    return code_line


def ls_does_contain(ls, subs):
    for s in ls:
        if (subs in s):
            return True
    return False


def b_begins_with_one_of_a(a, b):
    for i in a:
        if (b.startswith(i)):
            return True
    return False


# adds "throws Throwable" to the first function in class
def make_existing_function_throwable():
    global l_code

    line_number = 0
    while line_number < len(l_code):
        if (b_begins_with_one_of_a(IDENTIFIERS_FOR_FUNCTION, l_code[line_number].strip())):
            break
        line_number += 1

    if (line_number == len(l_code)):
        return

    if ("throws " in l_code[line_number]):
        if ("throws Throwable" in l_code[line_number]):
            return
        l_code[line_number] = l_code[line_number].replace("throws ", "throws Throwable, ")
        return

    if (l_code[line_number].strip()[-1] == "{"):
        l_code[line_number] = l_code[line_number].rstrip()[:-1] + " throws Throwable {"

    elif (l_code[line_number].strip()[-1] == ")"):
        if line_number < len(l_code) and "throws " in l_code[line_number + 1]:
            # 12private ColumnVector backSubstitution(ColumnVector y) throws Throwable
            # 13        throws MatrixException
            l_code[line_number + 1] = l_code[line_number + 1].replace("throws ", "throws Throwable, ")
            return
        l_code[line_number] = l_code[line_number].rstrip() + " throws Throwable "

    return


def search_var(class_name, var_name):
    global d_classes_to_add
    if class_name in d_classes_to_add:
        for item in d_classes_to_add[class_name]:
            if var_name == item[2]:
                return item
    if class_name == "global":
        return "VAR", global_vars[var_name], var_name


def tokenize_into_list(_str):
    #while re.search(r"\[[^\[\]]*?\]", _str):
    #    _str = re.sub(r"\[[^\[\]]*?\]", "", _str)
    if "[" in _str:
        _str = _str[:_str.find("[")]
    c_token = r'[a-zA-Z_][a-zA-Z_\d]*'
    pattern = re.compile(c_token)
    tokens = pattern.findall(_str)
    return tokens


def tokenizer(source_code):
    return


# def parse_quote(_str):
# def extract_error_tag(source_code_line, tag_line):
def find_next_char(desc, ptr):
    real_desc = desc[ptr:]
    c_token = r'([a-zA-Z_][a-zA-Z_\d]*)'
    if not re.match(c_token, real_desc):
        return ""
    first_word = re.match(c_token, real_desc).group(1)
    # print(first_word)
    if len(real_desc) <= len(first_word):
        return ""
    real_desc = real_desc[len(first_word):]
    for _char in real_desc:
        if _char and _char != " ":
            return _char


class Exception:

    def __init__(self, e, tag):
        self.l_e = [i for i in e.split('\n')]
        self.line_number = int(self.l_e[0].strip()[:self.l_e[0].find(":")]) - 1
        self.exception_desc = self.l_e[0].strip()[self.l_e[0].find("error: ") + 7:]
        if '(aka' in self.exception_desc:
            self.exception_desc = re.sub(r"\(aka '[^()]*'\) ", "", self.exception_desc)
        if (self.can_touch_this()):
            if ("error: " in self.l_e[0]):
                if tag:
                    self.analyse_type_missing_exception()
                else:
                    self.analyse_exception()
        # print("finish")

    def can_touch_this(self):
        return self.line_number not in dont_touch_list

    def add_to_dont_touch_list(self):
        global dont_touch_list
        dont_touch_list.add(self.line_number)

    def analyse_exception(self):
        global l_code
        global d_classes_to_add
        global reverse_method_to_class_mapping
        global exception_list_index

        global alias
        global identifiers
        global var_mapping
        global struct_order_graph
        global global_vars
        global ptr_tag

        global define_lines

        # print("analysis begin:", self.exception_desc)
        # unknown type name 'X'
        # Action: creat struct 'X'
        # array has incomplete element type 'X'
        # incomplete definition of type ''
        # variable has incomplete type 'enum comm'
        # subscript of pointer to incomplete type 'struct chng'
        if self.exception_desc.startswith("unknown type name ") \
                or self.exception_desc.startswith("array has incomplete element type") \
                or self.exception_desc.startswith("incomplete definition of type") \
                or self.exception_desc.startswith("subscript of pointer to incomplete type ") \
                or self.exception_desc.startswith("variable has incomplete type"):
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            struct_name = self.exception_desc.split("'")[1]
            if struct_name.startswith("struct "):
                struct_name = struct_name[7:]
            # print("unknown type name", struct_name)
            d_classes_to_add[struct_name] = set()
            return

        # invalid application of 'sizeof' to an incomplete type 'struct capaths'
        if self.exception_desc.startswith("invalid application of") and "to an incomplete type" in self.exception_desc:
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            struct_name = self.exception_desc.split("'")[3]
            if struct_name.startswith("struct "):
                struct_name = struct_name[7:]
            # print("unknown type name", struct_name)
            d_classes_to_add[struct_name] = set()
            return

        #  must use 'struct' tag to refer to type 'X'
        #  Action: typedef struct 'X'{}'X';
        if self.exception_desc.startswith("must use 'struct' tag to refer to type "):
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            struct_name = self.exception_desc.split("'")[3]
            if struct_name.startswith("struct "):
                struct_name = struct_name[7:]
            # print("'struct' tag", struct_name)
            alias[struct_name] = True
            return

        # no member named 'X' in 'Y'
        # no member named 'journalOff' in 'struct Pager'; did you mean 'journalHdr'?
        # Action: add UNKNown 'X' in 'Y'
        #  vect->n_vsize = size;
        #  ~~~~  ^
        if self.exception_desc.startswith("no member named "):
            struct_name = self.exception_desc.split("'")[3]
            if struct_name.startswith("struct "):
                struct_name = struct_name[7:]
            member_name = self.exception_desc.split("'")[1]

            # print(member_name, "not in", struct_name)
            #check pointer
            _, left_error_end = re.search(r"~+", self.l_e[2]).span()
            r_begin = self.l_e[2].find("^")
            if r_begin > left_error_end:
                call_ptr = self.l_e[1][left_error_end + 1:r_begin].strip()
                if call_ptr == "->":
                    new_struct_name = struct_name+"_t"
                    #d_classes_to_add[new_struct_name].add(('VAR', DEFAULT_STRUCT, member_name))
                    ptr_tag[struct_name] = True
            d_classes_to_add[struct_name].add(('VAR', DEFAULT_STRUCT, member_name))

            var_mapping[member_name] = struct_name
            return

        # use of undeclared identifier 'X'
        if self.exception_desc.startswith("use of undeclared identifier "):
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            identifier_name = self.exception_desc.split("'")[1]
            if identifier_name in d_classes_to_add:  # exist
                return
            # print("undeclared identifier", identifier_name)

            ptr = self.l_e[2].find("^")

            nxt_char = find_next_char(self.l_e[1], ptr)
            # print(nxt_char)
            if nxt_char == "*" or nxt_char.isalpha():
                d_classes_to_add[identifier_name] = set()
                return

            if (ptr - 1 >= 0 and self.l_e[1][ptr - 1] == '[') or (ptr - 2 >= 0 and self.l_e[1][ptr - 2] == '['):
                global_vars[identifier_name] = "int"
            else:
                global_vars[identifier_name] = MY_GLOBAL_HELPER_CLASS
            return
            # var_mapping[identifier_name] = "global"

        # invalid operands to binary expression
        # invalid operands to binary expression ('int' and 'bool' (aka 'struct bool'))
        # 4327:int accuracy = 100 - (mon->m_timed[MON_TMD_STUN] ? STUN_HIT_REDUCTION : 0);
        if self.exception_desc.startswith("invalid operands to binary expression "):
            # left_error, right_error = extract_error_tag()
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()
            right_error_begin, right_error_end = re.search(r"~+", self.l_e[2][left_error_end:]).span()
            right_error_begin += left_error_end
            right_error_end += left_error_end

            # print(self.l_e[2][left_error_end:])
            left_error_code = self.l_e[1][left_error_begin:left_error_end]
            right_error_code = self.l_e[1][right_error_begin:right_error_end]

            left_error_type = self.exception_desc.split("'")[1]
            right_error_type = self.exception_desc.split("'")[3]

            left_error_tokens = tokenize_into_list(left_error_code)
            right_error_tokens = tokenize_into_list(right_error_code)
            # print(left_error_tokens)
            if MY_GLOBAL_HELPER_CLASS in left_error_type:
                # change left type
                for token in left_error_tokens[-1::-1]:
                    # print(token)
                    if token in var_mapping:
                        struct_name = var_mapping[token]
                        item = search_var(struct_name, token)
                        d_classes_to_add[struct_name].remove(item)
                        d_classes_to_add[struct_name].add(('VAR', 'int', token))
                        # struct_order_graph.add_edge(right_error_type, struct_name)
                        break
                    elif token in global_vars:
                        global_vars[token] = "int"
                        break
            if MY_GLOBAL_HELPER_CLASS in right_error_type:
                # change right type
                for token in right_error_tokens[-1::-1]:
                    if token in var_mapping:
                        struct_name = var_mapping[token]
                        item = search_var(struct_name, token)
                        d_classes_to_add[struct_name].remove(item)
                        d_classes_to_add[struct_name].add(('VAR', 'int', token))
                        # struct_order_graph.add_edge(left_error_type, struct_name)
                        break
                    elif token in global_vars:
                        global_vars[token] = "int"
                        break
            if MY_GLOBAL_HELPER_CLASS not in left_error_type and MY_GLOBAL_HELPER_CLASS not in right_error_type:
                # add define
                if left_error_type in C_BASIC_DATA_TYPE:
                    define_lines.add("#define " + right_error_type + " " + left_error_type)
                elif right_error_type in C_BASIC_DATA_TYPE:
                    define_lines.add("#define " + left_error_type + " " + right_error_type)
            return

        # array subscript is not an integer
        # action:
        if self.exception_desc.startswith("array subscript is not an integer"):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()

            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)

            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    d_classes_to_add[struct_name].remove(('VAR', 'void*', token))
                    d_classes_to_add[struct_name].add(('VAR', "int", token))
                    # struct_order_graph.add_edge(right_error_type, struct_name)
                    break
            return

        # error: called object type 'struct UNKNOWN *' is not a function or function pointer

        # operand of type 'void' where arithmetic or pointer type is required
        if re.match("operand of type '.*' where arithmetic or pointer type is required", self.exception_desc):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()
            left_error_begin -= 1
            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)

            # print(left_error_tokens)
            # print(global_vars)
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    if "[10]" in item[1]:
                        target_type = "int[10]"
                    else:
                        target_type = "int"
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:
                    if "[10]" in global_vars[token]:
                        target_type = "int[10]"
                    else:
                        target_type = "int"
                    global_vars[token] = target_type
                    break

        # member reference base type 'void' is not a structure or union

        # statement requires expression of integer type ('void *' invalid)

        # cannot take the address of an rvalue of type

        # expression is not assignable

        # declaration of anonymous struct must be a definition

        # returning 'int' from a function with incompatible result type 'SQLITE_PRIVATE'
        if re.match(r"returning '.*' from a function with incompatible result type '.*'", self.exception_desc):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()

            left_error_code = self.l_e[1][left_error_begin - 1:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)

            target_type = self.exception_desc.split("'")[3]

            # print(left_error_tokens, target_type)
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    # print(target_type[7:], struct_name)
                    break
                elif token in global_vars:
                    global_vars[token] = target_type
            return

        # member reference type 'UNKNOWN' (aka 'struct UNKNOWN') is not a pointer;
        if re.match(r"member reference type .* is not a pointer", self.exception_desc):
            reference_type = self.exception_desc.split("'")[1]
            if reference_type in d_classes_to_add:
                ptr_tag[reference_type] = True
                return
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()

            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)
            # print(left_error_tokens)
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]

                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    d_classes_to_add[struct_name].add(('VAR', item[1] + "*", token))
                    break
                elif token in global_vars:
                    # item = ("VAR", global_vars[token], token)
                    global_vars[token] = global_vars[token] + "*"
                    break
            return

        # member reference type 'struct UNKNOWN *' is a pointer
        if re.match(r"member reference type '.*' is a pointer", self.exception_desc):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()

            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)
            # print(left_error_tokens)
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]

                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    d_classes_to_add[struct_name].add(('VAR', item[1].replace("*", ""), token))
                    break
                elif token in global_vars:
                    # item = ("VAR", global_vars[token], token)
                    global_vars[token] = global_vars[token].replace("*", "")
                    break
            return

        # initializing 'int' with an expression of incompatible type 'UNKNOWN'
        if re.match(r"initializing '.*' with an expression of incompatible type '.*'", self.exception_desc):
            target_type = self.exception_desc.split("'")[1]
            if " " in target_type:
                target_type = target_type.split(" ")[-1]

            # origin_type = self.exception_desc.split("'")[-1]

            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()

            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)

            # print(left_error_tokens, target_type)
            for token in left_error_tokens[-1::-1]:

                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:

                    global_vars[token] = target_type
                    break
            return

        # print("ok")

        # assigning to 'const char *' from incompatible type 'UNKNOWN' (aka 'struct UNKNOWN')
        if re.match(r"assigning to '.*' from incompatible type '.*'", self.exception_desc):
            target_type = self.exception_desc.split("'")[1]
            origin_type = self.exception_desc.split("'")[3]

            if MY_GLOBAL_HELPER_CLASS in target_type:
                # assigning to 'struct UNKNOWN' from incompatible type 'char'
                ptr = self.l_e[2].find("^")
                error_code = self.l_e[1][:ptr]
                target_type = origin_type
                left_error_tokens = tokenize_into_list(error_code)
                # print(left_error_tokens, target_type)
                for token in left_error_tokens[-1::-1]:
                    if token in var_mapping:
                        struct_name = var_mapping[token]
                        item = search_var(struct_name, token)
                        d_classes_to_add[struct_name].remove(item)
                        d_classes_to_add[struct_name].add(('VAR', target_type, token))
                        if "struct " in target_type:
                            target_type = target_type[7:]
                        struct_order_graph.add_edge(target_type, struct_name)
                        break
                    elif token in global_vars:
                        global_vars[token] = target_type
                        break
                return

            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()

            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)
            #print(left_error_tokens, target_type)
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:
                    global_vars[token] = target_type
                    break
            return

        # duplicate
        if self.exception_desc.startswith("duplicate"):
            line_number = 0
            # print(self.line_number)
            l_code[line_number] = "//" + l_code[line_number]

        # cannot combine with previous 'type-name' declaration specifier
        if self.exception_desc.startswith("cannot combine with previous 'type-name' declaration specifier"):
            ptr = self.l_e[2].find("^")
            # print(ptr, self.line_number)
            # print(l_code)
            # print(len(l_code))
            l_code[self.line_number] = l_code[self.line_number][ptr:]
            return

        # expected ';' after top level declarator
        if self.exception_desc.startswith("expected ';' after top level declarator"):
            code_line = l_code[self.line_number]
            words = code_line.split(' ')
            new_code_line = ""
            for word in words[1:]:
                new_code_line += word + " "
            new_code_line = new_code_line.strip()
            l_code[self.line_number] = new_code_line

        # subscripted value is not an array
        if self.exception_desc.startswith("subscripted value is not an array, pointer, or vector"):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()

            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)

            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    target_type = item[1]+"[10]"
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:
                    target_type = global_vars[token]+"[10]"
                    global_vars[token] = target_type
                    break

        # passing 'UNKNOWN' (aka 'struct UNKNOWN') to parameter of incompatible type 'unsigned long'
        if re.match("passing '.*' to parameter of incompatible type '.*'", self.exception_desc):
            src_type = self.exception_desc.split("'")[1]
            target_type = self.exception_desc.split("'")[3]

            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()
            left_error_begin -= 1
            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)

            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)

                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:

                    global_vars[token] = target_type
                    break
        # 1747.c:29:29: error: initializer element is not a compile-time constant

        # arithmetic on a pointer to an incomplete type
        if re.match("arithmetic on a pointer to an incomplete type '.*'", self.exception_desc):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()
            #left_error_begin -= 1
            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)
            target_type = "int"
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:
                    global_vars[token] = target_type
                    break
            return

        # called object type 'struct UNKNOWN *' is not a function or function pointer
        if re.match("called object type '.*' is not a function or function pointer", self.exception_desc):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()
            # left_error_begin -= 1
            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)
            #target_type = "int"
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    target_type = item[1] + "#ptr"
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:
                    target_type = global_vars[token] + "#ptr"
                    global_vars[token] = target_type
                    break
            return

        # expression is not an integer constant expression
        if re.match("expression is not an integer constant expression", self.exception_desc):
            left_error_begin, left_error_end = re.search(r"~+", self.l_e[2]).span()
            left_error_begin -= 1
            #print(left_error_begin, left_error_end)
            left_error_code = self.l_e[1][left_error_begin:left_error_end]

            left_error_tokens = tokenize_into_list(left_error_code)
            target_type = "int#constant"
            # print(left_error_tokens)
            # print(global_vars)
            for token in left_error_tokens[-1::-1]:
                if token in var_mapping:
                    struct_name = var_mapping[token]
                    item = search_var(struct_name, token)
                    d_classes_to_add[struct_name].remove(item)
                    # target_type = item[1] + "#ptr"
                    d_classes_to_add[struct_name].add(('VAR', target_type, token))
                    if "struct " in target_type:
                        target_type = target_type[7:]
                    struct_order_graph.add_edge(target_type, struct_name)
                    break
                elif token in global_vars:
                    # target_type = global_vars[token] + "#ptr"
                    global_vars[token] = target_type
                    break
            return

        # declaration of anonymous struct must be a definition
        if re.match("declaration of anonymous struct must be a definition", self.exception_desc):
            if "typedef struct" in self.l_e[1]:
                struct_type = self.l_e[1].split(" ")[2]
                if struct_type in d_classes_to_add:
                    d_classes_to_add.pop(struct_type)
            return


        #unsolved



    def analyse_type_missing_exception(self):
        global l_code
        global d_classes_to_add
        global reverse_method_to_class_mapping
        global exception_list_index

        global alias
        global identifiers
        global var_mapping
        global struct_order_graph
        global global_vars

        global define_lines

        # print("analysis begin1:", self.exception_desc)
        # unknown type name 'X'
        # Action: creat struct 'X'
        # array has incomplete element type 'X'
        # incomplete definition of type
        # variable has incomplete type 'enum comm'
        # subscript of pointer to incomplete type 'struct chng'
        if self.exception_desc.startswith("unknown type name ") \
                or self.exception_desc.startswith("array has incomplete element type") \
                or self.exception_desc.startswith("incomplete definition of type") \
                or self.exception_desc.startswith("subscript of pointer to incomplete type ") \
                or self.exception_desc.startswith("variable has incomplete type"):
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            struct_name = self.exception_desc.split("'")[1]
            if struct_name.startswith("struct "):
                struct_name = struct_name[7:]
            # print("unknown type name", struct_name)
            d_classes_to_add[struct_name] = set()
            return

        # invalid application of 'sizeof' to an incomplete type 'struct capaths'
        if self.exception_desc.startswith("invalid application of") and "to an incomplete type" in self.exception_desc:
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            struct_name = self.exception_desc.split("'")[3]
            if struct_name.startswith("struct "):
                struct_name = struct_name[7:]
            # print("unknown type name", struct_name)
            d_classes_to_add[struct_name] = set()
            return

        #  must use 'struct' tag to refer to type 'X'
        #  Action: typedef struct 'X'{}'X';
        if self.exception_desc.startswith("must use 'struct' tag to refer to type "):
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            struct_name = self.exception_desc.split("'")[3]
            if struct_name.startswith("struct "):
                struct_name = struct_name[7:]
            # print("'struct' tag", struct_name)
            alias[struct_name] = True
            return

        # use of undeclared identifier 'X'
        if self.exception_desc.startswith("use of undeclared identifier "):
            # struct_name = self.exception_desc.split(" ")[-1][1:-1]
            identifier_name = self.exception_desc.split("'")[1]
            # print("use of undeclared identifier", identifier_name)
            if identifier_name in d_classes_to_add:  # exist
                return
            ptr = self.l_e[2].find("^")

            nxt_char = find_next_char(self.l_e[1], ptr)
            # print(nxt_char)
            if nxt_char == "*" or nxt_char.isalpha():
                d_classes_to_add[identifier_name] = set()
                return
            # otherwise identifier is global variable
            global_vars[identifier_name] = MY_GLOBAL_HELPER_CLASS

        # cannot combine with previous 'type-name' declaration specifier
        if self.exception_desc.startswith("cannot combine with previous 'type-name' declaration specifier"):
            ptr = self.l_e[2].find("^")
            # print(ptr, self.line_number)
            # print(l_code)
            # print(len(l_code))
            l_code[self.line_number] = l_code[self.line_number][ptr:]
            return


def preprocess(file_path):
    new_content = "#include <stdio.h>\n" \
                  "#include <stdint.h>\n" \
                  "#include <string.h>\n" \
                  "#include <stdlib.h>\n" \
                  "#include <time.h>\n" \
                  "#include <aio.h>\n" \
                  "#include <pthread.h>\n" \
                  "#define bool int\n" \
                  "#define true 1\n" \
                  "#define false 0\n"
    # print(file_path)
    old_content = get_file_content(file_path).strip()

    old_content = re.sub(r"/\*.*?\*/","",old_content,flags=re.S)

    new_content += old_content

    write_file_content(file_path, new_content)


def reset_d_classes():
    global d_classes_to_add
    global exception_list_index
    d_classes_to_add = defaultdict(set)
    d_classes_to_add[DUMMY_RETURN_TYPE] = set()
    d_classes_to_add[MY_GLOBAL_HELPER_CLASS] = set()
    reverse_method_to_class_mapping = defaultdict()
    exception_list_index = 0


def add_tag(file_content):
    lines = file_content.split("\n")
    tagged_content = ""
    for idx in range(len(lines)):
        line = lines[idx]
        tagged_content += str(idx + 1) + line + "\n"
    return tagged_content


def handle_file(src_file_path, output_file_path, times_to_try_compile):
    global l_code
    global dont_touch_list
    # reset all data dict
    reset_d_classes()

    file_path = output_file_path

    duplicate_file(src_file_path, file_path)

    preprocess(file_path)

    l_code = get_file_content(file_path).split("\n")

    # make_existing_function_throwable()

    i = 0
    fw = open("info1.txt", 'a')
    fw1 = open("info2.txt", 'a')

    tag = True

    while (i < times_to_try_compile):
        # l_code = get_file_content(file_path).split("\n")  #add


        s = compile_file_and_get_output(file_path)
        # fw.write(add_tag(get_file_content(file_path)) + "\n")
        # fw.write(s + "\n")
        # print(s)
        # if i == 8:
        #     fw1.write(add_tag(get_file_content(file_path)) + "\n")
        #     fw1.write(s + "\n")
        l_errors = [l.strip() for l in s.split(file_path + ":")][1:]
        l_errors = [i for i in l_errors if (" error: " in i and " warning: " not in i)]

        num_errors = len(l_errors)

        if (num_errors == 0):
            return True
        # print("in")
        dont_touch_list = set()

        for j in range(len(l_errors)):
            Exception(l_errors[j], tag)
            # print("analysis end!")
        if i == 1:
            tag = False  # exe tag

        generated_code = get_new_code_to_add()

        if SEPARATOR_STRING in l_code:
            l_code = l_code[l_code.index(SEPARATOR_STRING) + 1:]

        new_code = "\n".join(generated_code + [SEPARATOR_STRING] + l_code)

        write_file_content(file_path, new_code)

        l_code = get_file_content(file_path).split("\n")

        i += 1
    remove_file_if_exists(output_file_path)
    return False


if __name__ == '__main__':

    l_code = []
    dont_touch_list = set()
    d_classes_to_add = defaultdict()
    reverse_method_to_class_mapping = defaultdict()
    alias = defaultdict()
    identifiers = defaultdict()
    exception_list_index = 0
    var_mapping = defaultdict()
    struct_order_graph = nx.DiGraph()
    global_vars = defaultdict()
    ptr_tag = defaultdict()
    allocate_value = 0

    define_lines = set()

    if (len(sys.argv) != 4):
        print("Usage:", sys.argv[0], "input_file", "output_file", "compile_tries")
        exit()

    inp_file_path = sys.argv[1]
    output_file_path = sys.argv[2]
    times_to_try_compile = int(sys.argv[3])

    result = False
    remove_file_if_exists(output_file_path)
    # runtime_error
    try:
        result = handle_file(inp_file_path, output_file_path, times_to_try_compile)
    except:
        print("Some error occured.")
        remove_file_if_exists(output_file_path)
    # if not result:
    #	ff = open("record.txt", 'a')
    #	ff.write(inp_file_path + "\n")
    print(result)
