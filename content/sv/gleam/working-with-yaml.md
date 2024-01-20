---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett dataformat för konfigurationsfiler och dataseriering. Programmerare använder YAML för att det är läsligt och kompatibelt med många programmeringsspråk.

## Hur gör man:
Gleam har ännu inte inbyggt stöd för YAML, så vi använder ett externt bibliotek för att hantera YAML-data.

```gleam
// Först, lägg till ett YAML-bibliotek i din `gleam.toml` fil.
// Exempelvis yaml_gleam, se https://hex.pm för det senaste.

import yaml_gleam

pub fn main() {
  let yaml_str = "
  en: Hello, world!
  sv: Hej, världen!
  "

  case yaml_gleam.decode(yaml_str) {
    Ok(data) -> io.println(data)
    Error(err) -> io.println(f"Error: {err}")
  }
}
```

Om allt går bra ska output vara något liknande detta:
```
{"en": "Hello, world!", "sv": "Hej, världen!"}
```

## Deep Dive
YAML introducerades i början av 2000-talet som ett enklare alternativ till XML. Andra populära alternativ idag är JSON och TOML. Eftersom Gleam är ett ungt språk, är stödet för YAML genom tredjepartsbibliotek. Implementationen använder Erlangs kraftfulla parsing och pattern matching för att omvandla YAML-strängar till Gleam-datastrukturer.

## See Also
- YAML officiella webbplats: https://yaml.org
- yaml_gleam bibliotek på Hex: https://hex.pm/packages/yaml_gleam
- Erlang's YAML stöd: https://github.com/yaml/erlang-yaml