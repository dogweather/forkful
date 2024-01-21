---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:52:57.450467-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut debug-information innebär att du temporärt visar kodens interna tillstånd, ofta via terminalen eller konsolen, för att förstå vad som händer under exekvering. Programmerare gör detta för att snabbt spåra och rätta till buggar.

## Så här gör du:
För att skriva ut debug-information i Gleam använder du `io.debug` funktionen. Nedan visas grundläggande exempel:

```Gleam
import gleam/io

pub fn main() {
  let message = "Hej, Gleam!"
  io.debug(message) // Skriver ut "Hej, Gleam!" till konsolen
}
```

Sample output till terminalen:

```
Hej, Gleam!
```

Att använda `debug` är enkelt och rakt på sak. Du kan även skriva ut datatyper och strukturer för att se deras innehåll:

```Gleam
import gleam/io

pub fn main() {
  let data = [1, 2, 3, 4, 5]
  io.debug(data) // Skriver ut listan till konsolen
}
```

Sample output:

```
[1, 2, 3, 4, 5]
```

## Djupdykning
I tidiga programmeringsdagar användes ofta loggmeddelanden eller små lampor på maskinpaneler för att debugga. Med tiden har verktyg som REPL (Read-Eval-Print Loop) och moderna debuggers utvecklats, men enkel utskriftsdebugging används fortfarande flitigt, tack vare dess enkelhet.

Varianter på att skriva ut debug-information inkluderar loggningsbibliotek som erbjuder mer avancerad funktionalitet som filtrering av loggnivåer och formatering. I Gleam är `io.debug` en enkel variant som skiljer sig från loggningsbibliotek genom att det är direkt och omedelbart – perfekt för snabb feedback under utveckling.

När det gäller implementationen skriver `io.debug` ut till STDERR istället för STDOUT för att inte blanda samman applikationens output med debug-meddelanden.