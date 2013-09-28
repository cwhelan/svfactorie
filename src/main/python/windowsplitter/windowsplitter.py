__author__ = 'cwhelan'

import sys
import pybedtools

windows = sys.argv[1]
features = sys.argv[2]
out_dir = sys.argv[3]

def intervals_identical(i1, i2):
    return i1.chrom == i2.chrom and i1.start == i2.start and i1.end == i2.end

featureBT = pybedtools.BedTool(features)
windowBT = pybedtools.BedTool(windows)
prev_window = None
outfile = None
bins_in_windows = featureBT.intersect(b=windowBT, wo=True)
num_fields = bins_in_windows.field_count(1)
for bin_in_window in bins_in_windows:
    sys.stderr.write(str(bin_in_window))
    current_window = pybedtools.Interval(bin_in_window[num_fields - 4], int(bin_in_window[num_fields - 3]), int(bin_in_window[num_fields - 2]))
    # sys.stderr.write("current window = {0}\n".format(current_window))
    # if prev_window is None or not intervals_identical(current_window, prev_window):
    #     sys.stderr.write("new window\n")
    #     if outfile is not None:
    #         sys.stderr.write("closing old window..")
    #         outfile.close()
    #     sys.stderr.write("opening file\n")
    outfile = open("{0}/{1}:{2}-{3}.bed".format(out_dir, bin_in_window[num_fields - 4], bin_in_window[num_fields - 3], bin_in_window[num_fields - 2]), "a")
    outfile.write("\t".join(map(str, bin_in_window[0:(num_fields - 4)])) + "\n")
    outfile.close()
    prev_window = current_window