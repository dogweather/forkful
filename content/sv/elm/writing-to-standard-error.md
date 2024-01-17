---
title:                "Skriva till standardfel"
html_title:           "Elm: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När vi programmerar i Elm, kan vi behöva skriva till standard error. Detta är ett sätt att skicka meddelanden till användaren som indikerar att något har gått fel. Det är ett viktigt verktyg för att kunna felsöka och hantera eventuella fel som kan uppstå vid körning av vårt program.

## Hur man:

I Elm kan vi skriva till standard error genom att använda funktionen `Debug.crash` och skicka med ett meddelande som en sträng. Exempelvis:

```Elm
main =
    Debug.crash "Det här är ett felmeddelande"
```

Detta kommer att skrivas ut i konsolfönstret när programmet körs:

```
Uncaught Debug.crash: Det här är ett felmeddelande
```

## Deep Dive:

Att skriva till standard error har funnits i programmering under lång tid och används för att göra det enklare att debugga program. Alternativet till att skriva till standard error är att skriva till standard output, vilket används för att skriva ut önskad information för användaren. Det finns även andra metoder att använda för felsökning, som till exempel loggning till en fil.

I Elm så skickas meddelandet till standard error genom den inbyggda funktionen `Console.error`, som i bakgrunden konverterar meddelandet till en JSON-sträng. Det är också möjligt att ändra denna standardinställning och skicka meddelandet till standard output istället genom att använda funktionen `Debug.log`.

## Se även:

- [Elm dokumentation om standard error](https://package.elm-lang.org/packages/elm/core/latest/Debug#crash)
- [Blogginlägg om standard error i Elm](https://medium.com/@francisdb/debugging-elm-with-standard-error-b3796de87d47)