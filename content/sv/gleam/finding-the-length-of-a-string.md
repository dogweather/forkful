---
title:                "Hitta längden på en sträng"
html_title:           "Gleam: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att hitta längden på en sträng är en viktig teknik för programmerare. Det innebär helt enkelt att räkna antalet tecken som finns i en sträng. Det här är användbart för att göra jämförelser, validera inmatad data och utföra olika typer av manipulation på text. 

## Hur man gör:
För att hitta längden på en sträng i Gleam använder man funktionen `String.length`. Här är ett exempel på hur man kan använda det i en kod:

```Gleam
let sträng = "Hej, världen!"
let längd = String.length(sträng)

io.println(längd) // kommer att skriva ut 13
```

## Djupdykning:
Att hitta längden på en sträng är en grundläggande uppgift som har funnits sedan de tidigaste programmeringsspråken. I Gleam finns det också andra sätt att hitta längden på en sträng, såsom `sträng.length()`, men det är en mindre vanligt använd funktion. Några alternativ till Gleam för att hitta längden på en sträng är Java med funktionen `length()` och Python med `len()`.

Implementationen av `String.length` i Gleam är baserad på Unicode-teckenet som standard, men man kan också ange att man vill räkna tecken baserat på byte genom att använda `Byte.length`. Detta kan vara användbart om man arbetar med icke-Unicode-tecken.

## Se även:
Om du vill lära dig mer om strängar och hur de används i Gleam, rekommenderas att läsa dokumentationen. Det finns också flera användbara tutorials och videor som visar hur man använder `String.length` och andra strängfunktioner.