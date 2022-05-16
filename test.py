import os
import subprocess

print("")
print("  ___________________________________________")
print("  |                                         |")
print("  |   Welcome to the Cheez testing suite!   |")
print("  |                                         |")
print("  |_________________________________________|")
print("")

TEST_DIR = "./test/"
tests = []

# fetch test cases
for f in os.listdir(TEST_DIR):
    if f.endswith(".cheez"):
        tests.append(os.path.splitext(f)[0])

failed = 0

# clean up first
subprocess.run(["make clean > /dev/null 2>&1"], shell=True)
for test_file in tests:

    print("[+] Running test \"{}\"...".format(test_file))
    # build
    subprocess.run(["ocamlbuild -pkgs llvm,llvm.analysis cheez.native > /dev/null 2>&1"], shell=True)
    # feed in code
    subprocess.run("./cheez.native < test/{}.cheez > test/{}.out".format(test_file, test_file), shell=True)
    # generate output
    subprocess.run("lli test/{}.out > test/{}.txt".format(test_file, test_file), shell=True)
    res = subprocess.run(
        [
            "diff",
            "test/{}.txt".format(test_file),
            "test/{}.output".format(test_file)
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    
    # print(len(res.stderr))
    # print(len(res.stdout))

    if len(res.stderr) == 0 and len(res.stdout) == 0:
        print("[+] test \"{}\" PASSED.".format(test_file))
    else:
        print("[!] test \"{}\" FAILED. (!!!)".format(test_file))
        failed += 1

    print("")



print("")
if failed == 0: 
    print("[+] {}/{} test cases PASSED".format(len(tests), len(tests)))
else:
    print("[!] {}/{} test cases FAILED (!!!)".format(failed, len(tests)))

print("  ___________________________________________")
print("                 CLEANING UP                 ")
subprocess.run("make clean && rm test/*.txt > /dev/null 2>&1", shell=True)
print("                 TESTING COMPLETE            ")
print("  ___________________________________________")