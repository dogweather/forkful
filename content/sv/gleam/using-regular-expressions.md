---
title:                "Använda reguljära uttryck"
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Reguljära uttryck, eller regex, är mönster som hjälper dig hitta textbaserad information. Programmerare använder dem för att effektivisera textbearbetning - det är snabbt, kraftfullt och sparar tid.

## How to:
Gleam använder Rust's `regex` bibliotek. Installera först med `gleam add regex`. Här är grunderna:

```gleam
import gleam/regex

// Hitta första matchningen
fn find_example() {
  let re = regex.new("hej").unwrap()
  regex.find(re, "hej värld")
  |> io.debug // Skriver ut: "Some(Match(start: 0, end: 3))"
}

// Se om det finns någon matchning
fn is_match_example() {
  let re = regex.new("värld").unwrap()
  regex.is_match(re, "hej värld")
  |> io.debug // Skriver ut: true
}

// Ersätt text
fn replace_example() {
  let re = regex.new("värld").unwrap()
  regex.replace(re, "hej värld", "sverige")
  |> io.debug // Skriver ut: "hej sverige"
}

find_example()
is_match_example()
replace_example()
```

## Deep Dive
Reguljära uttryck har använts sedan 1950-talet och populariserades i Unix-verktyg på 1970-talet. Alternativ till regex inkluderar strängmanipuleringsfunktioner och parser generators, men de är oftast mer komplexa. Implementationen i Gleam via Rust's `regex` är snabb och säker, tack vare Rust's minneshantering och typsystem.

## See Also
- Rust's regex-bibliotek: [https://docs.rs/regex](https://docs.rs/regex)
- Ett interaktivt regex-verktyg: [https://regex101.com/](https://regex101.com/)
