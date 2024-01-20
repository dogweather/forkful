---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck (regex) är ett kraftfullt verktyg för att matcha och bearbeta strängar. Det används av utvecklare för att spara tid och för att effektivisera koden genom att hitta specifika mönster av tecken i en textvaror.

## Hur man:
Här är några exempel på Gleam-kod som visar hur man använder regex:

```Gleam 
import gleam/regex

let phones = regex.from_string("(07|08)[0-9]{8}") |> should.from_result!

"0734567890, 0834567890, 0934567890"
|> regex.find_all(phones)
|> should.equal(Ok(["0734567890", "0834567890"]))
```

Koden ovan matchar alla telefonnummer som börjar med 07 och 08 och har totalt 10 siffror. De telefonnummer som matchar detta mönster returneras sedan.

## Fördjupning
Reguljära uttryck har funnits sedan 1950-talet och är ett etablerat verktyg inom programmering. Alternativ till regex finns, exempelvis strängmanipulationsfunktioner, men de når inte upp till regex flexibilitet och effektivitet.

Implementera regex i Gleam kan vara lite komplicerat eftersom du behöver matcha strängen exakt. Du behöver alltid börja med att importera regex-modulen.

## Se även
Vill du veta mer? Här är några användbara länkar:

Ovanstående länkar erbjuder djupgående studiematerial och verktyg för att vidareutveckla din förståelse och färdigheter inom regex.