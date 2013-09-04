#!/usr/bin/env python

import sys
import changePointDetection

window_file = sys.argv[1]

for line in open(window_file, "r"):
    fields = line.rstrip().split("\t")
    chrom = fields[0]
    start = fields[1]
    end = fields[2]
    segFile = "%s.inserts.txt.gz" % chrom
    changePointDetection.detect_change_points(int(start), int(end), segFile, 100, 7, sys.stdout, printBed=True, chrom=chrom, resolution=25)
