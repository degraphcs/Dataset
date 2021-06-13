#!/usr/bin/python3
import argparse
import subprocess
import os

RUNNER = "wrapper_runner.py"
OUT_PATH = "fixed/output.java"
N_TRIES = 10

def wrapper(inp_file_path, out_file_path=OUT_PATH, times_to_try_compile=N_TRIES):
	subprocess.run(['./'+RUNNER, inp_file_path, out_file_path, str(times_to_try_compile)], stderr=subprocess.PIPE).stderr.decode('utf-8')

def tmp_run(inp_file_path, out_file_path):
	#print("beginerror",["clang", inp_file_path, "-emit-llvm -S -c -o", out_file_path])
	#cmd = "clang " + inp_file_path + " -emit-llvm -S -c -o " + out_file_path
	cmd = ["clang", inp_file_path, "-emit-llvm", "-S", "-c", "-o", out_file_path]
	#a = os.system(cmd)
	print("ok")
	return subprocess.run(cmd, stderr=subprocess.PIPE).stderr.decode('utf-8')

def run():
	input_prefix = "benchmark/bigclonebenchdata_partial/"
	out_prefix = "benchmark/output/e"
	for idx in range(100000000):
		input_file = input_prefix + str(idx) + ".java"
		if not os.path.exists(input_file):
			continue
		out_file = out_prefix + str(idx) + ".java"
		TIMES_TO_TRY_COMPILE = 20
		wrapper(input_file, out_file, TIMES_TO_TRY_COMPILE)

if __name__ == '__main__':
	
	parser = argparse.ArgumentParser()

	parser.add_argument('-f', required=True, help="path to input c code snippet")
	parser.add_argument('-o', default=OUT_PATH, help="path to output c file. default=" + OUT_PATH)
	parser.add_argument('-n', type=int, default=N_TRIES, help="max number of times to try compile. default=10")
	args = parser.parse_args()

	INPUT_FILE = args.f
	OUTPUT_FILE = args.o
	TIMES_TO_TRY_COMPILE = int(args.n)
	

	wrapper(INPUT_FILE, OUTPUT_FILE, TIMES_TO_TRY_COMPILE)


