import sys
import changePointDetection

window_file = sys.args[0]

for line in sys.open(window_file, "r"):
    (chrom, start, end) = line.rstrip().split("\t")
    segFile = "%.inserts.txt.gz" % chrom
    changePointDetection.detect_change_points(int(start), int(end), segFile, 100, 7, sys.stdout)