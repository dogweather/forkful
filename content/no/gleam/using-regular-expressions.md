---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk lar deg søke og manipulere tekst med mønstergjenkjenning. Programmerere bruker det for å effektivisere tekstbehandling, validering og transformasjon.

## Hvordan:
```gleam
import gleam/regex

fn main() {
  let re = regex.from_string("[a-z]+").unwrap()
  assert Ok(some) = regex.find(re, "hello123 world")
  assert some == "hello"
}
```
Output:
```
"hello"
```
## Dypdykk
Historisk sett stammer regulære uttrykk fra teoretisk lingvistikk og er en sentral del av Unix. Alternativer inkluderer strengmanipuleringsfunksjoner, men disse mangler ofte fleksibiliteten og ytelsen til regulære uttrykk. Implementeringen i Gleam vender seg til Rust-biblioteket 'regex', som gir robust og høytytende mønstersøk.

## Se Også
- [Gleam Regex Module Docs](https://hexdocs.pm/gleam_stdlib/gleam/regex/)
- [RegexOne: Interactive Regex Tutorial](https://regexone.com/)
- [MDN Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)