---
date: 2024-01-20 17:55:19.417068-07:00
description: "S\xE5 h\xE4r g\xF6r du: Argument p\xE5 kommandoraden har funnits sedan\
  \ de tidiga dagarna av Unix. De till\xE5ter skript att vara flexibla och \xE5teranv\xE4\
  ndbara. Man brukar\u2026"
lastmod: '2024-04-05T22:50:52.402618-06:00'
model: gpt-4-1106-preview
summary: "Argument p\xE5 kommandoraden har funnits sedan de tidiga dagarna av Unix."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## Så här gör du:
```Bash
#!/bin/bash
# script.sh

echo "Argument nummer ett: $1"
echo "Argument nummer två: $2"

if [ $# -eq 0 ]; then
    echo "Inga argument gavs."
else
    echo "Antalet givna argument är: $#"
fi
```
Körning och utskrift:
```Bash
$ bash script.sh Älg Ost
Argument nummer ett: Älg
Argument nummer två: Ost
Antalet givna argument är: 2
```

## Djupdykning
Argument på kommandoraden har funnits sedan de tidiga dagarna av Unix. De tillåter skript att vara flexibla och återanvändbara. Man brukar hantera argument med `$1`, `$2`,..., där `$1` är det första argumentet. För att fånga alla argument använder man `$@` eller `$*`, medan `$#` ger antalet argument.

Det finns mer avancerade alternativ som `getopts` eller `getopt` som stödjer switchar (som `-h` eller `--help`). Med dessa kan du skapa robusta och användarvänliga gränssnitt för dina skript.

Implementationen av dessa är standardiserade via POSIX, vilket betyder att din kod kan flyttas mellan olika system utan problem.

## Se även:
- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
- POSIX standarden: https://pubs.opengroup.org/onlinepubs/009695399/utilities/getopts.html
