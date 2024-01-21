---
title:                "Läsa in kommandoradsargument"
date:                  2024-01-20T17:56:04.986683-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in kommandoradsargument innebär att ditt program kan ta emot input direkt när det startar. Programmerare gör detta för att göra sina program mer flexibla och användbara för olika scenarier.

## How to:
I Gleam kan du hantera kommandoradsargument med `os.args()` funktionen. Här är ett enkelt exempel på hur det kan se ut:

```gleam
import gleam/io
import gleam/list
import gleam/os

pub fn main() {
  let args = os.args()
  match list.drop(args, 1) { // Första argumentet är sökvägen till programmet.
    [] -> 
      io.println("Inga argument givna.")
    [first_arg | _] -> 
      io.println("Första argumentet är: " ++ first_arg)
  }
}
```

Om du kör detta program så här `gleam run mitt_program Hej Världen`, blir utskriften:

```
Första argumentet är: Hej
```

## Deep Dive
Att läsa kommandoradsargument är ett arv från de tidiga dagarna av programmering där interaktionerna ofta var textbaserade. Modernt språk som Gleam behåller denna funktion för kompatibilitet och verktygsbyggande. Alternativ till argument på kommandoraden inkluderar konfigurationsfiler och miljövariabler, men inget är lika snabbt och enkelt för mindre uppgifter. Implementationen av argumentläsning i Gleam är en wrapper runt funktioner som erbjuds av dess host-språk (t.ex. Erlang), vilket syftar till en enkel och robust hantering av kommandoradsdata.

## See Also
För vidare läsning och fördjupning, se Gleams officiella dokumentation:


Så, nu är det bara att börja utforska och ha så skoj med din kodning i Gleam!