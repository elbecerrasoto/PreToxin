TARGZ = data.tar.gz

data: $(TARGZ)
	tar -xvf $<

.PHONY help:
help:
	less Makefile

# PHONY Avoids circular dependency
.PHONY targz:
targz:
	tar -czvf $(TARGZ) data

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".")'

.PHONY clean:
	rm -rf data
