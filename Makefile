DATA = data
PTTG = $(DATA)/pttg.tsv
TARGZ = data.tar.gz

.PHONY help:
help:
	less Makefile

.PHONY test:
test: $(PTTG)
	./asserts.R

$(PTTG): $(TARGZ)
	tar -xvf $<

# PHONY Avoids circular dependency
.PHONY targz:
targz:
	tar -czvf $(TARGZ) $(DATA)

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".")'

# .PHONY clean:
# dangerous
# as I sometimes modify the input tables
#	rm -rf data
