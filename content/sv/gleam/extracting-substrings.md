---
title:                "Extrahera delsträngar"
date:                  2024-01-20T17:45:53.204055-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
I programmering är att extrahera delsträngar processen för att plocka ut specifika bitar av text från en större sträng. Det används för att manipulera och analysera data, som att hämta användarnamn från e-postadresser eller produktkoder från en URL.

## How to:
Gleam erbjuder flera metoder för att extrahera delsträngar. Följande exempel visar grunderna:

```gleam
import gleam/string

fn main() {
  let text = "Hej, Sverige!"
  let hello = string.slice(text, 0, 3) // "Hej"
  let sweden = string.slice(text, 5, 13) // "Sverige"

  // visa resultaten
  io.debug(hello)
  io.debug(sweden)
}
```
Kör den här koden och du får:

```
"Hej"
"Sverige"
```

## Deep Dive
Delsträngsextrahering är inte nytt i programmeringsvärlden. I äldre språk som C var det mer komplicerat och riskabelt. Moderna språk, som Gleam, förenklar processen och ökar säkerheten genom tydlig syn på strängens begränsningar och felhantering.

I Gleam använder man ofta `string.slice` för att skapa delsträngar. Det finns alternativa metoder som `string.left` och `string.right` beroende på behov. Att utföra operationen effektivt kan variera beroende på strängens storlek och vilken del som ska extraheras. Gleam hanterar dessa detaljer åt dig, vilket gör det mindre felbenäget jämfört med lägre nivå språk.

## See Also
- Rusts "slice"-koncept, något liknande i system-nivå programmering: [The Rust Programming Language - Slices](https://doc.rust-lang.org/book/ch04-03-slices.html)
- Elixir, ett annat funktionellt programmeringsspråk på Erlang VM: [Elixir String Documentation](https://hexdocs.pm/elixir/String.html)