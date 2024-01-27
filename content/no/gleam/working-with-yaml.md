---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML står for "YAML Ain't Markup Language". Programmere bruker det for å håndtere konfigurasjonsfiler og datautveksling for letthet og lesbarhet.

## How to:
Gleam har ennå ikke et dedikert YAML-bibliotek, så vi må håndtere YAML som strenger. Bruk andre språk med biblioteker, som `serde_yaml` i Rust, for tung YAML-behandling.

```gleam
// Gleam-kode for enkel YAML-håndtering kommer når støtte er tilgjengelig
```

## Deep Dive
YAML lansert i 2001, er ofte foretrukket fremfor JSON for konfigurasjon fordi det er mer menneskelesbart. Alternativer inkluderer JSON og TOML. Implementasjon i Gleam er i vente, men interop med Erlang eller Elixir kan tilby midlertidige løsninger.

## See Also
- YAML offisiell side: https://yaml.org/
- `serde_yaml` for Rust: https://docs.rs/serde_yaml
- Gleam-lang offisiell dokumentasjon: https://gleam.run/book
