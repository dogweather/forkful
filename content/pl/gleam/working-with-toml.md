---
title:                "Praca z TOML"
date:                  2024-01-26T04:22:15.153340-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z TOML oznacza parsowanie i generowanie plików TOML (Tom's Obvious, Minimal Language) za pomocą kodu. Programiści używają TOML dla łatwych do odczytu plików konfiguracyjnych i serializacji danych, dzięki jasnej semantyce i zgodności z konwencjonalnymi typami danych.

## Jak to zrobić:
Gleam nie posiada wbudowanego wsparcia dla TOML, więc będziesz potrzebować zewnętrznej biblioteki. Na przykład:

```gleam
// Zakładając, że masz bibliotekę do parsowania TOML:
import toml/{Parser, Encoder}

// Parsuj zawartość TOML
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Użyj sparsowanych danych
match parsed {
  Ok(data) -> "Dane sparsowane pomyślnie!"
  Error(_) -> "Nie udało się sparsować danych."
}

// Generuj zawartość TOML z struktury danych Gleam
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Przykładowy wynik:

```
Dane sparsowane pomyślnie!
```

## Dogłębna analiza
TOML został wydany w 2013 roku przez Toma Preston-Wernera. Jego celem: być bardziej czytelnym i prostszym niż XML oraz mniej skomplikowanym niż YAML dla konfiguracji plików. Pomimo prostoty jest solidny dla strukturyzowanych danych, oferując wyraźną i łatwą do zrozumienia składnię. Alternatywami są JSON, YAML i INI, ale minimalizm i przejrzysta składnia TOML często przeważają dla plików konfiguracyjnych. Implementacja TOML w Gleam obejmuje dwie główne czynności: parsowanie TOML do natywnych struktur danych i serializacja natywnych struktur danych do TOML. Większość bibliotek TOML dla Erlanga lub Elixira może być używana w Gleam dzięki jego współpracy z językami BEAM, zapewniając płynną integrację w projektach Gleam.

## Zobacz także
- Specyfikacja języka TOML: [https://toml.io/en/](https://toml.io/en/)
- Parser TOML dla Erlanga: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML na GitHubie: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)