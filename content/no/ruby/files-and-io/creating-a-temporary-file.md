---
date: 2024-01-20 17:41:11.141568-07:00
description: "How to: I Ruby kan du bruke `Tempfile` biblioteket for \xE5 lage og\
  \ h\xE5ndtere midlertidige filer."
lastmod: '2024-03-13T22:44:41.352763-06:00'
model: gpt-4-1106-preview
summary: "I Ruby kan du bruke `Tempfile` biblioteket for \xE5 lage og h\xE5ndtere\
  \ midlertidige filer."
title: Opprette en midlertidig fil
weight: 21
---

## How to:
I Ruby kan du bruke `Tempfile` biblioteket for å lage og håndtere midlertidige filer:

```Ruby
require 'tempfile'

Tempfile.create('mittEksempel') do |tempfil|
  tempfil.write('Heisann, denne teksten blir midlertidig lagret her.')
  tempfil.rewind
  puts tempfil.read  # => "Heisann, denne teksten blir midlertidig lagret her."
end
# Filen er nå lukket og slettet.
```

## Deep Dive:
Midlertidige filer har vært et konsept siden de tidlige dagene av programmering, og brukes for å unngå å bruke unødig minne, eller når man bare trenger data midlertidig. Det finnes alternativer, som å bruke in-memory datastrukturer, men disse kan bruke mer minne og er ikke optimale for store datamengder. `Tempfile` i Ruby er en wrapper rundt klassen `File`, som automatisk tar seg av opprettelse, håndtering og sletting av midlertidige filer på en trygg måte. Tempfiler lagres i `/tmp` eller en tilsvarende mappe definert av operativsystemet, og får unike navn for å unngå konflikter.

## See Also:
- [Ruby-Doc for Tempfile](https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby-Doc for File](https://ruby-doc.org/core-2.5.1/File.html)
- [Ruby-Doc for IO](https://ruby-doc.org/core-2.5.1/IO.html)
