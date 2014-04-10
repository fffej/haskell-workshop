%.html: %.pandoc Makefile
	pandoc --standalone --self-contained -t slidy -o $@ $<

all: Slides.html
