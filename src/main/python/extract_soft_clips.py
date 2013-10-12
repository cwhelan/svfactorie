#!/usr/bin/env python

import sys
import fileinput
import re

def print_scpos(chrom, scpos):
    bin = scpos - scpos % 25
    print "\t".join([chrom, str(bin), "1"])

for line in fileinput.input():
    fields = line.split("\t")
    chrom = fields[2]
    pos = fields[3]
    cigar = fields[5]
    if 'S' in cigar:
        cigar_ops = re.sub(r'([0-9]+)([A-Z])', r'\1|\2|', cigar).split("|")
        left_clip = False
        right_clip = False
        if cigar_ops[1] == 'S' and int(cigar_ops[0]) > 4: 
            # found soft clip at start of read
            left_clip = True
            scpos = int(pos) + int(cigar_ops[0])
            print_scpos(chrom, scpos)
        if cigar_ops[len(cigar_ops) - 2] == 'S' and  int(cigar_ops[len(cigar_ops) - 3]) > 4:
            # found soft clipping at end of read
            right_clip = True
            seqlen = sum([int(cigar_ops[i]) for i in xrange(0, len(cigar_ops) - 2, 2)])
            if left_clip:
                seqlen = seqlen - int(cigar_ops[0])            
            scpos = int(pos) + seqlen - int(cigar_ops[len(cigar_ops) - 3])
            print_scpos(chrom, scpos)
