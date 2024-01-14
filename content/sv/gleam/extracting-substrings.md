---
title:                "Gleam: Extrahera subträngningar"
simple_title:         "Extrahera subträngningar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera delsträngar är en viktig del av programmering eftersom det låter dig manipulera och arbeta med text på ett mer effektivt sätt. Genom att bara välja de delar av en sträng som du behöver kan du spara tid och resurser i din kod.

## Hur man gör det

För att extrahera en delsträng från en huvudsträng i Gleam, använder man funktionerna `String.slice` och `String.slice_unchecked`. Dessa funktioner tar emot tre parametrar: huvudsträngen, startpositionen och slutpositionen för den önskade delsträngen.

Exempelvis, om vi har en sträng "Hello World" och vill extrahera "World" från den, skulle vi använda `String.slice` och definiera startpositionen som 6 (index börjar från 0) och slutpositionen som 10. Kodexemplet nedan visar hur detta skulle se ut i Gleam:

```
Gleam
import String

let huvudsträng: String = "Hello World"
let delsträng: String = String.slice(huvudsträng, 6, 10)

assert delsträng == "World"
```

I detta exempel använder vi `assert` för att kontrollera att den extraherade delsträngen faktiskt är det vi förväntade oss.

## Djupdykning

Det finns flera olika scenarier där man kan behöva extrahera delsträngar, och därför finns det flera olika sätt att använda funktionerna `String.slice` och `String.slice_unchecked`.

Om du vet att huvudsträngen alltid kommer att ha en viss längd, kan du använda `String.slice_unchecked` för att undvika att programmet kraschar om positionerna är utanför huvudsträngen. Denna funktion är snabbare eftersom den inte behöver göra några säkerhetskontroller.

Om du vill extrahera en del av en sträng baserat på ett villkor, som t.ex. om strängen innehåller ett visst tecken eller ord, kan du använda funktionerna `String.contains` och `String.find` för att hitta positionen och sedan använda `String.slice` för att extrahera den önskade delsträngen.

## Se även

Här är några användbara länkar för att lära dig mer om hur man extraherar delsträngar i Gleam:

- [Gleams dokumentation om strängmanipulering](https://gleam.run/books/tutorial/text_manipulation.html)
- [Officiell Gleam-läroplan](https://gleam.run/books/education/index.html)
- [Kodexempel för strängmanipulering i Gleam](https://github.com/gleam-lang/gleam/blob/master/lib/std/string.gleam)