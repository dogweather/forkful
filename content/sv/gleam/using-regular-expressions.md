---
title:                "Användning av reguljära uttryck"
html_title:           "Gleam: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions är ett kraftfullt verktyg som gör det möjligt att söka och manipulera textmönster i dina program. Om du vill göra avancerade sökningar eller ersätta textsträngar effektivt är det ett verktyg du bör ha i din programmeringsverktygslåda.

## Hur man använder

Först och främst måste du importera RegularExpressions biblioteket i ditt Gleam-projekt. Sedan kan du använda dess funktioner i dina kodblock.

``` Gleam
import RegularExpressions
```

### Sökning efter ett mönster

För att söka efter ett specifikt mönster i en textsträng kan du använda funktionen `Regex.find`. Den tar in två argument - mönstret du vill söka efter och textsträngen som ska sökas igenom. Till exempel, om du vill hitta alla förekomster av ordet "Gleam" i en textsträng kan du använda följande kod:

``` Gleam
let text = "Gleam är ett fantastiskt programmeringsspråk."

let mönster = "Gleam"

let resultat = Regex.find(mönster, text)

IO.print(resultat) // Utskrift: Success("Gleam")
```

### Manipulera textsträngar

Förutom att söka efter mönster, kan du också använda regular expressions för att manipulera textsträngar. Funktionen `Regex.replace` kan användas för att ersätta en del av en textsträng med en annan. Den tar in tre argument - mönstret som ska ersättas, ersättningstexten och textsträngen som ska manipuleras.

``` Gleam
let text = "Gleam är ett fantastiskt programmeringsspråk."

let mönster = "programmeringsspråk"

let ersättning = "språk"

let resultat = Regex.replace(mönster, ersättning, text)

IO.print(resultat) // Utskrift: Success("Gleam är ett fantastiskt språk.")
```

## Deep Dive

Regular expressions består av olika specialtecken och metakaraktärer som används för att skapa sökmönster. Till exempel, `.` matchar vilken tecken som helst, `*` matchar noll eller flera förekomster av det föregående tecknet och `+` matchar en eller flera förekomster av det föregående tecknet. Det är viktigt att ha en bra förståelse för dessa specialtecken för att kunna skapa effektiva sökmönster.

En annan viktig del av regular expressions är gruppdramläggning, vilket gör det möjligt att söka efter delar av ett mönster och använda dessa delar i ersättningssträngen. Detta är särskilt användbart om du vill manipulera textsträngar på olika sätt baserat på ett visst mönster.

## Se även

- [Gleams officiella dokumentation för RegularExpressions](https://gleam.run/libraries/regular_expressions.html)
- [Mer praktiska exempel på hur man använder RegularExpressions](https://github.com/gleam-lang/gleam/blob/master/lib/std/regex/regex_test.gleam)