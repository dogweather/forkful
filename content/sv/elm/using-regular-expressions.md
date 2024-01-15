---
title:                "Att använda reguljära uttryck"
html_title:           "Elm: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regular expressions är ett kraftfullt verktyg för att söka, ersätta och manipulera textsträngar i programkod. Genom att använda reguljära uttryck kan du snabbt och effektivt redigera stora mängder text på ett precist och strukturerat sätt.

## Hur man gör
För att använda reguljära uttryck i Elm, behöver du först importera `Regex` modulen. Sedan kan du definiera ett reguljärt uttryck med hjälp av `Regex.regex` funktionen och mata in det i `Regex.find` eller `Regex.replace` beroende på vad du vill göra. Här är ett exempel på hur du skulle söka efter ett visst mönster i en textsträng:

```elm
import Regex

-- Definiera det reguljära uttrycket
pattern = Regex.regex "[0-9]+"

-- Textsträng att söka i
text = "Det finns 300 enhörningar i skogen"

-- Sök efter mönster med Regex.find
Regex.find pattern text
--> Just (Regex.match "300" (Regex.at [6, 7, 8] text))
```

Som du kan se returnerar `Regex.find` en `Just` typ med en `Regex.match` variabel som innehåller det matchade mönstret. Om det inte finns någon matchning, skulle det returnera `Nothing`. Du kan också använda `Regex.replace` för att ersätta en textsträng med ett annat mönster.

## Djupdykning
Reguljära uttryck har ett väldigt specifikt syntax som kan verka förvirrande först. Men när du väl lärt dig grunderna, kommer du snabbt att inse hur användbara de är. Här är några saker att tänka på när du arbetar med reguljära uttryck i Elm:

- `|` används för att separera olika möjliga matchningar. Till exempel `[a-z]|[0-9]` kommer att matcha antingen en bokstav i alfabetet eller en siffra.
- `*` och `+` betyder att föregående matchning ska upprepas noll eller flera gånger. `*` betyder också att mönstret kan vara frånvarande helt. Till exempel `ab*c` skulle matcha `ac`, `abc` eller `abbbc`.
- `.` representerar en generell karaktär som kan vara vilken som helst.
- Om du vill matcha en speciell karaktär som normalt används i regex-syntax, som `*` eller `|`, måste du använda `\\` före karaktären för att undvika att den tolkas som en del av syntaxen.

Det finns många olika funktioner inom `Regex` modulen som du kan använda för att bygga komplexa reguljära uttryck. Se till att läsa dokumentationen för mer information och exempel.

## Se även
- [`Regex` modulen i Elm Dokumentation](https://package.elm-lang.org/packages/elm/regex/latest)
- [RegExr - ett online-verktyg för att testa och lära sig reguljära uttryck](https://regexr.com/)
- [Reguljära uttryck - tutorial på W3Schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)