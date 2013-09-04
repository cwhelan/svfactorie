import sys
import changePointDetection

window_file = sys.args[0]

current_seg_file = None
fp = None
for line in sys.open(window_file, "r"):
    (chrom, start, end) = line.rstrip().split("\t")
    segFile = "%.inserts.txt.gz" % chrom
    if segFile != current_seg_file:
        fp = gzip.open(segFile)
        current_seg_file = segFile
    changePointDetection.detect_change_points(int(start), int(end), fp, 100, 7, sys.stdout)