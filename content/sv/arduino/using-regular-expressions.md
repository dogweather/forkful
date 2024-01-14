---
title:                "Arduino: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Att använda reguljära uttryck i Arduino-programmering kan hjälpa till att effektivisera kodning genom att möjliggöra sökning och ersättning av specifika mönster i en sträng. Det är också en användbar teknik för att validera och filtrera olika typer av data.

## Så här gör du

För att använda reguljära uttryck i Arduino behöver du först inkludera biblioteket för reguljära uttryck med hjälp av följande kodrad:

```
#include <Regex.h>
```

Sedan kan du använda funktioner som `regexMatch()` eller `regexReplace()` för att söka och ersätta delar av en sträng baserat på ett specifikt mönster. Till exempel, om du vill hitta alla siffror i en sträng, kan du använda följande kod:

```
String str = "Det finns 123 äpplen i korgen.";
regexReplace(str, "\\d+", ""); // Tar bort alla siffror från strängen
```

Det finns också inbyggda regex-funktioner som `match()`, `find()`, `start()` och `end()` som kan hjälpa till att hitta matchningar och extrahera information från en sträng. Det finns många olika mönster och uttryck som kan användas för att söka och manipulera data, så det är en god idé att göra lite forskning och experiment för att lära sig mer.

## Djupdykning

En reguljärt uttryck består av ett mönster av tecken som beskriver en sträng. Det kan innehålla vanliga tecken, såsom bokstäver och siffror, men också specialtecken som används för att definiera mönster och villkor. Till exempel kan `.` användas för att matcha vilket tecken som helst och `+` för att matcha ett eller flera förekomster av ett tecken.

Det finns också metatecken som kan användas för att definiera grupper av tecken, såsom `[a-z]` för att matcha alla små bokstäver eller `[^a-z]` för att matcha alla tecken som inte är små bokstäver. Andra användbara metatecken inkluderar `?` för att matcha ett eller inget av ett tecken och `*` för att matcha noll eller flera förekomster av ett tecken.

Det finns också mönster som kan användas för att matcha speciella typer av tecken, till exempel `\w` för att matcha alfanumeriska tecken och `\$` för att matcha dollartecken. Det är viktigt att vara noga med användningen av specialtecken och metatecken för att säkerställa att du får den önskade matchningen.

## Se även

- [Regex biblioteket för Arduino](http://www.arduinolibraries.info/libraries/regex)
- [Reguljära uttryck - Wikipedia](https://sv.wikipedia.org/wiki/Regulj%C3%A4ra_uttryck)
- [15 minuter för att lära sig reguljära uttryck](https://www.youtube.com/watch?v=sa-TUpSx1JA)