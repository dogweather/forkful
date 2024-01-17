---
title:                "Kapitalisera en sträng"
html_title:           "Elm: Kapitalisera en sträng"
simple_title:         "Kapitalisera en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kapitalisering av en sträng i programmering innebär att man gör om den första bokstaven i varje ord i strängen till stor bokstav. Detta görs för att göra strängen läsbarare och mer estetiskt tilltalande. Det är en vanlig konvention som följs av många programmerare.

## Så här gör man:

### Elm:

```
import String

String.capitalize "detta är en sträng" 
--> "Detta är en sträng"
```

### Utdata:

```
Detta är en sträng
```

## Djupdykning:

### Historisk bakgrund:

Att kapitalisera en sträng har varit en långvarig konvention inom programmeringsvärlden. Det har sitt ursprung i att göra text lättare att läsa för människor och har sedan dess blivit standardiserat i många programmeringsspråk.

### Alternativ:

En alternativ metod för att kapitalisera en sträng är att använda funktionen `String.toUpper`, vilket gör hela strängen till versaler istället för bara den första bokstaven. Detta kan dock påverka läsbarheten i vissa fall.

### Implementeringsdetaljer:

I Elm finns funktionen `String.capitalize` som tar en sträng och returnerar en ny sträng med den första bokstaven i varje ord kapitaliserad. Detta görs genom att först splitta strängen till en lista av ord, sedan använda funktionen `List.map` för att göra om varje första bokstaven till stor bokstav och slutligen använda funktionen `String.join` för att sätta ihop strängen igen.

## Se även:

[Elm Docs - String](https://package.elm-lang.org/packages/elm/core/latest/String)