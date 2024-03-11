---
date: 2024-01-26 04:13:46.325975-07:00
description: "Read-Eval-Print Loppen (REPL) \xE4r en enkel, interaktiv programmeringsmilj\xF6\
  \ som tar emot enskilda anv\xE4ndarinput, utv\xE4rderar dem och returnerar resultatet\u2026"
lastmod: '2024-03-11T00:14:11.179195-06:00'
model: gpt-4-0125-preview
summary: "Read-Eval-Print Loppen (REPL) \xE4r en enkel, interaktiv programmeringsmilj\xF6\
  \ som tar emot enskilda anv\xE4ndarinput, utv\xE4rderar dem och returnerar resultatet\u2026"
title: "Anv\xE4nda en interaktiv skal (REPL)"
---

{{< edit_this_page >}}

## Vad & Varför?
Read-Eval-Print Loppen (REPL) är en enkel, interaktiv programmeringsmiljö som tar emot enskilda användarinput, utvärderar dem och returnerar resultatet till användaren. Elm-programmerare använder REPL för snabba experiment, felsökning eller att lära sig språket.

## Hur man gör:
Elm levereras inte med en integrerad REPL. Du kan dock använda `elm repl` från din kommandorad för att starta en Elm-session efter att ha installerat Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : Lista nummer
```

I denna session, efter att ha importerat List-funktioner, dubblerade vi talen i en lista och fick resultatet omedelbart.

## Fördjupning
Elms REPL kan verka begränsad jämfört med de från vissa andra språk som Python eller JavaScript, eftersom Elm är ett kompilerat språk inriktat på att producera webbapplikationer. Historiskt har Elm fokuserat på hela applikationer snarare än skriptning eller skalinteraktioner.

Alternativ till Elms REPL inkluderar `elm-live` och online-editorer som Ellie där du kan se ändringar i kod reflekteras i realtid i en webbläsare.

När det gäller implementeringen, kompilerar Elm REPL snuttar av Elm-kod till JavaScript i bakgrunden, vilket låter dig köra Elm interaktivt. Detta skiljer sig från REPL:er för tolkade språk, som inte behöver detta kompileringssteg. Elm REPL är också avskalad för att hålla kärnspråket lättviktigt och fokuserat.

## Se även
- Elms officiella guide om interaktivitet: https://guide.elm-lang.org/interop/
- Ellie, en online Elm-lekplats: https://ellie-app.com/new
- `elm-live`, en flexibel utvecklingsserver för Elm: https://www.elm-live.com/
