#! /usr/bin/env python

import os
import argparse

parser = argparse.ArgumentParser()

parser.add_argument("-d", "--domain", help="domain to run gw or stp")
parser.add_argument("-a", "--algorithm", help="algorithm to run wa* or dps")
parser.add_argument("-b", "--bound", help="suboptimal bound a real number")

args = parser.parse_args()

domain = args.domain
algorithm = args.algorithm
bound = float(args.bound)

print "Making Korf instances for " + algorithm + " algorithm and " + domain + " domain\n with bound " + str(bound)

korf_instances = open('master', 'r')

for line in korf_instances:
    instance = line.split(" ")
    instance_number = instance[0]
    print instance_number
    instance_file = open(str(algorithm)+"/"+instance_number, 'w')
    instance_file.write(domain + "\n")
    instance_file.write(algorithm + "\n")
    instance_file.write(str(bound) + "\n")
    instance_file.write("4\n4\n")
    counter = 1
    for cell in instance[1:]:
        if counter % 4 == 0:
            instance_file.write(str(cell) + "\n")
        else:
            instance_file.write(str(cell) + "\t")
        counter += 1
    instance_file.close()

korf_instances.close()
print "Done."

