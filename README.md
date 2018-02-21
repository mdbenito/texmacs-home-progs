# My TeXmacs config #

This is just a collection of TeXmacs initialisation files and routines which I haven't committed to the trunk. Some are too specific or personal, others I just didn't find the time for it.

## What's here ###

In no particular order:

* Browsing to-do tags with the focus bar.
* List all labels in the focus bar when in a reference tag.
* Trees to item lists (abandoned).
* Splitting equations into equation arrays automatically and back.
* Autocomplete glue functions (**deprecated:** Philippe committed something better to the trunk).
* Lots of shortcuts (e.g. Fraktur letters, beamer presentations,...).
* ...

## Set up ##

Clone into submodules (e.g. [tm2md](https://bitbucket.org/mdbenito/tm2md)) upon first cloning with

```
git submodule update --init --recursive
```

For tm2md create a symbolic link to it named markdown:
```
ln -s convert/tm2md convert/markdown
```

Then just link your `~/.TeXmacs/progs` to this project's folder.

## License ##

Icon borrowed from [exercism.io](http://exercism.io/languages/scheme/about).

Everything else is GPLv3.
