# `fridge`

`fridge` is a simple approach to managing data analysis projects in
**R**.

## Background

`fridge` assumes that data analysis projects are managed using a
hierarchical folder structure. Helper functions are kept seperated
from the code that specifies the final models etc., in a `lib`
directory.

Objects that need long computation times to be created are stored as
binary files (`*.rda`) in a `cache` directory and are reused if needed
rather than recreated each time the analyses are run.

## Installation

```r
install.packages("devtools")
library(devtools)
install_github("cszang/fridge")
```

## Setup

Create a `lib` directory inside the main project folder, and populate with auxilliary code needed for e.g. munging data from various sources, creating special plots, ...

## Usage

### Caching

To create cached objects, use `freeze()` or its infix operator `%<f-%` for assignment and caching in one step:

```r
freeze("a_big_sum", sum(1:2000))
# the same as
a_big_sum %<f-% sum(1:2000)
```

This will assign `sum(1:2000)` to a new object `a_big_sum`, and cache
it for later use. The next time the exact same call is made from the
same or another script, the assignment is not evaluated, but the
cached object is loaded. It will also store the checksum of the
expression used to create the object and issue a warning when the
expression has changed from the cached version of the object.
    
To "forget" an assignment including cached data, use `forget()`.

### Compare checksums during assigning read in file

To assure that the object created while reading in data is identical
to previous runs of the analysis, the SHA1 sum of the object can be
stored and compared to the previous version. If the SHA1 of the
current object diverges from the SHA1 sum of the same object from a
previous run, a warning is issued and the old SHA1 sum is
retained. After adapting the code to the new version of the data,
delete the SHA1 sum corresponding to the object and re-create the
object.

This is probably most helpful when your data is too big for Git(Hub),
Git LFS is not an option, or the same big data files are used for
multiple projects (e.g., large climate data sets of several GB in
size).

```r
e %<c-% read_table("some_file.txt")
```

### Requesting objects

`request` checks for the existance of an object in the current
session. If the object is not defined, it will try to load it into the
session from the project's `cache` directory.

### Empty cache

All cached objects can be deleted in one go using
`empty_cache()`. This will, however, not "forget" the objects in the
current workspace.

### Loading library functions

Auxilliary code from the `lib` directory can easily be sourced into
the R session using `load_lib()`, which can either load all files from
the `lib` folder (default), or only specified ones, which have to be
only referenced to by their base name without extension.

