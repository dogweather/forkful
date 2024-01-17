---
title:                "Kontrollera om en katalog existerar."
html_title:           "Fish Shell: Kontrollera om en katalog existerar."
simple_title:         "Kontrollera om en katalog existerar."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
 Att kolla om en mapp finns är en vanlig uppgift för programmerare. Detta gör man för att säkerställa att en mapp är tillgänglig innan man fortsätter med en kodsekvens som är beroende av den.

## Så här gör du:
Detta är hur du kollar om en mapp finns i Fish Shell:

```
if test -d /mappnamn
   echo "Mappen finns!"
else
   echo "Mappen finns inte."
endif
```

Om mappen existerar kommer det första uttrycket att skrivas ut, annars kommer det andra.

## Deep Dive:
Historiskt sett har programmerare använt kommandot `test` för att utföra villkorsbaserade kontroller i Unix-baserade operativsystem. Detta kommando har senare inkluderats i shell-program, inklusive Fish Shell.

Alternativet till `test -d` i Fish Shell är kommandot `count`. Detta är ett mer generellt kommando som kan användas för att kontrollera andra typer av filer, inte bara mappar.

Implementeringsdetaljer: Fish Shell använder sig av POSIX-standarder och använder `test` -kommandot för att utföra en villkorsbaserad kontroll.

## Se även:
- Fish Shell dokumentation: https://fishshell.com/docs/current/index.html
- POSIX-standarder: https://en.wikipedia.org/wiki/POSIX