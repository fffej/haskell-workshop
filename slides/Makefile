%.html: %.pandoc Makefile
	pandoc --standalone -t slidy -o $@ $<

all: Slides.html
