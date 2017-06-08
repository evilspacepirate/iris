## Synopsis

Uses a web camera to detect motion and saves images when motion is
detected.

## Motivation

A desire to construct a surveillance apparatus.

## Dependencies

### Compilation

* GNATMAKE

### Run Time

* ImageMagick
* Feh

## Installation

1. Install ImageMagic and Feh.
2. Build Iris using gnatmake
`gnatmake iris`

## Operational Description

Iris continuously captures web camera images and uses ImageMagick to
compare the latest two images. If the latest two images differ by a
percentage threshold, then the latest two images and the next 10 images
will be saved to the disk.

Command line configuration of various parameters will be added as the
author finds the time. Until then, modify the constants and rebuild as
desired.

## License

Internet Systems Consortium (ISC)
