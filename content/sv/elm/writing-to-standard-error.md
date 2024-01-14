---
title:                "Elm: Skrivning till standardfel"
simple_title:         "Skrivning till standardfel"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av Elm-programmering eftersom det ger en enkel och effektiv metod för att skriva felmeddelanden och felsökningsmeddelanden.

## Hur du gör det

För att skriva till standard error i Elm, används funktionen `Debug.crash` tillsammans med felmeddelandet som en sträng. Till exempel:

```Elm
Debug.crash "Det här är ett felmeddelande"
```

Detta kommer att skriva ut `"Det här är ett felmeddelande"` till standard error.

## Fördjupning

Att använda `Debug.crash` för att skriva till standard error är särskilt användbart för att hitta fel i större program. Istället för att bara få det sista felet som genererades kan du skriva ut flera felmeddelanden för att hjälpa till med felsökningen.

En annan fördel med att skriva till standard error är att du kan ange en anpassad callback-funktion som körs istället för att skriva till standard error direkt. På så sätt kan du styra var och hur felmeddelandet ska skrivas ut.

## Se även

- Elm Dokumentation: System.Debug
- Elm Dokumentation: Basics via System.crash