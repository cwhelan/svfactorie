#!/usr/bin/env python

import sys
import gzip
import changePointDetection

window_file = sys.argv[1]
data_file_suffix = sys.argv[2]
obs_cutoff = int(sys.argv[3])
segment_size = int(sys.argv[4])

current_seg_file = None
fp = None
for line in open(window_file, "r"):
    fields = line.rstrip().split("\t")
    chrom = fields[0]
    start = fields[1]
    end = fields[2]
    segFile = "%s.%s" % (chrom, data_file_suffix)
    if segFile != current_seg_file:
        fp = gzip.open(segFile)
        current_seg_file = segFile
    changePointDetection.detect_change_points(int(start) - (segment_size + 25), int(end) + segment_size, fp, segment_size, obs_cutoff, sys.stdout, printBed=True, chrom=chrom, resolution=25)

