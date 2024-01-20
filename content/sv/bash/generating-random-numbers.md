---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga nummer är en process att skapa en sekvens av nummer som inte kan förutsägas bättre än av en slumpmässig chans. Programmerare använder slumpmässig nummergenerering för att skapa en unik sessionsnyckel, för spellogik, säkerhet och simuleringar.

## Hur man gör:
I Bash kan du generera ett slumpmässigt nummer med $RANDOM-variabeln. Här är hur:

```Bash
echo $RANDOM
```
Kör scriptet, och du får ett slumpmässigt nummer mellan 0 och 32767.

För att få ett nummer inom ett specifikt intervall, använd modulus-operatorn (%). Till exempel, för att få ett tal mellan 1 och 100, kan du göra så här:

```Bash
echo $(( RANDOM % 100 + 1 ))
```

## Djup Dykning
RANDOM är en intern Bash-funktion som ger en pseudoslumpig helheltsvärde varje gång den anropas. Historiskt skapades det för att skapa enkel slumpmässig funktionalitet inbyggd i skalet, utan behov av externa verktyg.

Ett alternativ till att använda $RANDOM kan vara att använda /dev/urandom-enheten i Linux, som ger en oändlig ström av slumpmässiga bytes. Det är dock överkill för de flesta applikationer, och $RANDOM är mer än tillräckligt.

Tänk på att $RANDOM ger pseudoslumpmässiga nummer baserade på en seed; det är inte avsett för stark kryptografisk användning. För sådana fall, använd /dev/random eller /dev/urandom i kombination med dd och od verktyg.

## Se även
För mer detaljerad information, kör `man bash` och leta efter RANDOM under SHELL BUILTIN COMMANDS.

Du kan också läsa mer om användningen av /dev/urandom här: https://www.2uo.de/myths-about-urandom/ 

Besök GNU Bash Manual för mer om inbyggda variabler: https://www.gnu.org/software/bash/manual/bash.html#Shell-Variables