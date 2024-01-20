---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att hitta längden på en sträng innebär att bestämma antalet enheter i den, vanligtvis tecken. Programmerare gör det för att bearbeta data mer effektivt, särskilt inom textbearbetning och analys.

## Så här gör du:
I Gleam kan du hitta längden på en sträng med hjälp av `length` funktionen.

```Gleam
let minStrang = "Hej Sverige!"
let minStrangLangd = string.length(minStrang)
```

Exempel output:

```Gleam
12
```

Här är "Hej Sverige!" strängen 12 tecken lång, inklusive mellanslag och utropstecken.

## Djup dykning
Att hitta längden på en sträng är ett grundläggande koncept inom programmering och har använts sedan de tidigaste dagarna av programmeringsspråk.

Det finns alternativ till `string.length`-metoden i andra språk, till exempel `len()` i Python eller `length()` i Java. Vilken du väljer att använda beror på det specifika programmeringsspråket och problemet du försöker lösa.

Vad gäller implementeringsdetaljer i Gleam, då Gleam är ett starkt och statiskt typat språk byggt på Erlang, behandlar det också strängar som listor av bokstäver. Så, när du begär en strängs längd, itererar Gleam igenom listan för att räkna antalet element.

## Se också
För mer information om strängar i Gleam, kolla in följande länkar:

1. [Gleam's String Documentation](https://gleam.run/book/tour/strings.html)
2. [Why use String length method](https://stackoverflow.com/questions/37756389/why-use-string-length-method)
3. [Learn Gleam: Strings and their length](https://gleam.run/tour/strings.html)