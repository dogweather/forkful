---
title:                "Sammenslåing av strenger"
date:                  2024-01-20T17:34:50.748859-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenslåing av strenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
I programmering er det å konkatere strenger å sette dem sammen til én. Vi gjør det for å bygge setninger, lage dynamiske meldinger, eller kombinere data på en meningsfull måte.

## How to:
Gleam gjør det enkelt. Se på disse eksemplene:

```Gleam
fn main() {
  let greet = "Hei"
  let name = "Ola"
  let message = greet ++ ", " ++ name ++ "!"
  message
}

// Output: "Hei, Ola!"
```

```Gleam
fn assemble_sentence(subject: String, verb: String, object: String) -> String {
  subject ++ " " ++ verb ++ " " ++ object ++ "."
}

fn main() {
  let sentence = assemble_sentence("Katten", "jag", "musen")
  sentence
}

// Output: "Katten jag musen."
```

Du legger bare `++` mellom strengene du vil sette sammen.

## Deep Dive
Konkatenering av strenger er ikke nytt. Historisk har det variert fra språk til språk, noen brukte operatører som `+` eller funksjoner som `concat()`. I Gleam, og mange funksjonelle språk, brukes `++`.

Alternativer? I noen språk kan du bruke string interpolation eller template literals, hvor du kan sette inn variabler direkte i strengen. Gleam støtter ikke dette nå, men det ryktes at framtidige versjoner kan innføre liknende funksjoner.

Implementasjonsdetaljer? Når konkatenering skjer, skapes en ny streng i minnet. Dette kan være dyrt hvis det gjøres mye, så noen språk har spesialiserte strukturer som `StringBuilder` i Java for å forbedre ytelsen. Gleam håndterer dette effektivt internt så du sjelden trenger å bekymre deg for ytelse her.

## See Also
For mer om Gleam og strenger:
- En guide til funksjonelle språk prinsipper: [https://wiki.haskell.org/Functional_programming](https://wiki.haskell.org/Functional_programming)
- String konkatenering i andre språk: [https://rosettacode.org/wiki/String_concatenation](https://rosettacode.org/wiki/String_concatenation)
