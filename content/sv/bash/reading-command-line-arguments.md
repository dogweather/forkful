---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kommandoradsargument är input som skrivs in direkt i terminalen när man startar ett script eller program. Detta gör det möjligt för programmerare att manövrera och styra programmet på en dynamisk, användardriven sätt.

## Hur man gör:

För att läsa kommandoradsargument i Bash använder vi speciella variabler. Se exempel nedan:

```Bash
#!/bin/bash

# Skriv ut det första argumentet
echo "Första argumentet: $1"

# Skriv ut det andra argumentet
echo "Andra argumentet: $2"

# Skriv ut alla argument
echo "Alla argument: $@"
```

Körning och output:

```Bash
$ ./myscript.sh Hej Världen
Första argumentet: Hej
Andra argumentet: Världen
Alla argument: Hej Världen
```

## Djupdykning

Kommandoradsargument är en av grunderna för Unix-stilen på datorgränssnitt, och det har funnits sedan 1970-talet. Alternativen inkluderar att läsa input under körning eller från filer, men argument är ofta det enklaste och mest direkt.

Bash-hanteringen av argument är lite annorlunda än andra språk. Till exempel kan `$0` användas för att visa namnet på själva scriptet. `$#` ger antalet argument och `$*` listar alla argument som en enda sträng.

## Se även 

För mer information om kommandoradsargument i bash, se dessa länkar:

- [Bash Programming Introduction by Mike G](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- [Linux Shell Scripting Tutorial (LSST) v2.0](https://bash.cyberciti.biz/guide/Pass_arguments_into_a_function)