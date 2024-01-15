---
title:                "Praca z yaml"
html_title:           "Gleam: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego?

Tworzenie aplikacji to nie tylko pisanie kodu, ale także zarządzanie konfiguracją. W tym celu wykorzystuje się język YAML, który pomaga w łatwej i czytelnej definicji ustawień i opcji aplikacji.

## Jak to zrobić?

Instalacja biblioteki `gleam-yaml` jest prosta - wystarczy dodać ją do `gleam.toml` i zaimportować moduł w swoim kodzie. Następnie używając funkcji `load` lub `load_file` można wczytać plik YAML do zmiennej w postaci mapy lub listy.

```Gleam
import gleam/yaml

let config = yaml.load_file("config.yml")
```

Możliwe jest również tworzenie, modyfikowanie i zapisywanie nowych plików YAML przy użyciu funkcji `dump` lub `dump_file`. Dzięki temu mamy pełną kontrolę nad konfiguracją naszej aplikacji.

```Gleam
import gleam/yaml

let new_config = {
  "version": "1.0",
  "port": 3000,
  "database": {
    "host": "localhost",
    "username": "admin",
    "password": "pass123"
  }
}

yaml.dump_file(new_config, "new_config.yml")
```

## Dogłębne omówienie

Język YAML jest znacznie przyjemniejszy w użyciu niż np. format JSON, ponieważ pozwala na wykorzystanie wcięć i formatowania tekstu. Sposób, w jaki jest zapisywany plik YAML, przypomina dokument lub listę, dzięki czemu jest czytelniejszy dla programistów. Warto również pamiętać o konwencjach dotyczących nazw, czyli stosowaniu camelCase lub snake_case, aby ułatwić wprowadzanie i odczytywanie danych.

## Zobacz także

- [Oficjalna dokumentacja biblioteki Gleam YAML](https://gleam.run/packages/gleam-yaml/1.0.0)
- [Porównanie YAML i JSON](https://www.computronik.com.pl/tekst/cejowy-article-pi9896)