---
title:                "Praca z yaml"
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

YAML to format tekstowy do przechowywania danych, podobny do JSON, ale bardziej czytelne dla człowieka. Programiści używają go do konfiguracji projektów i danych, dzięki łatwościom w czytaniu i edycji.

## How to: (Jak to zrobić:)

Gleam obecnie nie posiada wbudowanego wsparcia dla YAML, więc trzeba używać zewnętrznych bibliotek. Poniżej znajduje się przykład, jak to zrobić z fikcyjną biblioteką `gleam_yaml`.

```gleam
import gleam_yaml

fn main() {
  let data = """
  name: Jan Kowalski
  age: 30
  languages:
    - Polski
    - Angielski
  """
  
  let parsed_data = gleam_yaml.parse(data)
  case parsed_data {
    Ok(value) -> value
    Error(err) -> err
  }
}
```

**Wynik:**
```
Ok(#{
  "name": "Jan Kowalski",
  "age": 30,
  "languages": ["Polski", "Angielski"]
})
```

## Deep Dive (Głębsze spojrzenie)

YAML (YAML Ain't Markup Language) powstał w 2001 roku jako język łatwy do zrozumienia przez człowieka i maszyny. Alternatywą jest JSON, szybszy w przetwarzaniu, ale mniej czytelny. Implementacja obsługi YAML w Gleam zależy od zewnętrznych bibliotek. Przy obsłudze YAML ważne jest bezpieczne zarządzanie złożonością danych oraz unikanie podatności związanych z deserializacją.

## See Also (Zobacz również)

- Oficjalna strona YAML: [https://yaml.org](https://yaml.org)
- Dokumentacja Gleam: [https://gleam.run](https://gleam.run)
- Porównanie JSON i YAML: [https://json2yaml.com/](https://json2yaml.com/)