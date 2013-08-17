genome=/Users/cwhelan/genomes/1kg/hg18/human_b36_male_chr2.fasta.fai
resolution=25

deletions_hap1=chr2_deletions_hap1_gte40.bed.gz
deletions_hap2=chr2_deletions_hap2_gte40.bed.gz

insertions_hap1=chr2_insertions_hap1_gte40.bed.gz
insertions_hap2=chr2_insertions_hap2_gte40.bed.gz

numeric_features=crf_bwa_training_data.bed.gz
annotations=chr2_simple_repeats.bed.gz chr2_repeatmasker.bed.gz
labels=$(deletions_hap1) $(deletions_hap2) deletion_flanks.bed.gz insertion_locations_hap1.bed.gz insertion_locations_hap2.bed.gz insertion_flanks.bed.gz

windowsplitter=~/Documents/SVfactorie/src/main/python/windowsplitter/windowsplitter.py

all: window_files

bins.bed.gz: $(genome)
	bedtools makewindows -g $(genome) -w $(resolution) | awk 'OFS="\t" {print $$1,$$2,$$3-1}' | gzip -c > $@

deletion_flanks.bed.gz: $(genome) $(deletions_hap1)
	flankBed -i $(deletions_hap1) -g $(genome) -b 100 | gzip -c > $@

insertion_flanks.bed.gz: $(genome) $(insertions_hap1)
	flankBed -i $(insertions_hap1) -g $(genome) -b 100 | cut -f1-3 | gzip -c > $@

deletion_windows.bed: $(genome) $(deletions_hap1)
	gzip -cd $(deletions_hap1) | slopBed -pct -b 1 -i stdin -g $(genome) > $@

insertion_locations_hap1.bed.gz: $(insertions_hap1)
	gzip -cd $< | cut -f1-3 | gzip -c > $@

insertion_locations_hap2.bed.gz: $(insertions_hap2)
	gzip -cd $< | cut -f1-3 | gzip -c > $@

insertion_windows.bed: $(genome) $(insertions_hap1)
	gzip -cd $(insertions_hap1) | slopBed -b 500 -i stdin -g $(genome) > $@

event_windows.bed: deletion_windows.bed insertion_windows.bed
	cat $^ | gshuf | cut -f1-3 > $@

event_windows_sorted.bed: event_windows.bed
	cat $< | sort -k1,1 -k2,2n > $@

training_data.bed.gz: ~/Documents/SVfactorie/target/scala-2.10/classes/edu/ohsu/sonmezsysbio/svfactorie/BedFileZipper.class bins.bed.gz $(numeric_features) $(annotations) $(labels)
	scala -classpath ~/Documents/SVfactorie/target/scala-2.10/classes edu.ohsu.sonmezsysbio.svfactorie.BedFileZipper bins.bed.gz $(numeric_features) \
	    $(annotations) $(labels) | gzip -c > training_data.bed.gz

window_files: clean_windows $(windowsplitter) training_data.bed.gz event_windows_sorted.bed
	rm windows/*
	python $(windowsplitter) event_windows_sorted.bed training_data.bed.gz windows
	touch window_files

clean_windows:
	rm windows/*
	touch clean_windows

clean: clean_windows
	rm training_data.bed.gz
	rm window_files
