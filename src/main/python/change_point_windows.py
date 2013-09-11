#!/usr/bin/env python

import sys
import gzip
import changePointDetection

window_file = sys.argv[1]

current_seg_file = None
fp = None
for line in open(window_file, "r"):
    fields = line.rstrip().split("\t")
    chrom = fields[0]
    start = fields[1]
    end = fields[2]
    segFile = "%s.inserts.txt.gz" % chrom
    if segFile != current_seg_file:
        fp = gzip.open(segFile)
        current_seg_file = segFile
    changePointDetection.detect_change_points(int(start) - 125, int(end) + 100, fp, 100, 7, sys.stdout, printBed=True, chrom=chrom, resolution=25)

