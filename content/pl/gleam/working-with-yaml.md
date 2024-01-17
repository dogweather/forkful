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

## Czym jest YAML i dlaczego jest ważny dla programistów?
YAML to format pliku używany do przechowywania danych w formie czytelnej dla człowieka. Jest on szczególnie popularny w świecie programowania, ponieważ ułatwia przechowywanie i udostępnianie konfiguracji oraz danych w aplikacjach.

## Jak tego używać?
Poniżej znajdują się przykładowe kody oraz wynikowe wyjście, pokazujące, jak używać YAML w języku Gleam.

```
Gleam.program(
  "greetings_example",
  optional_extra_inputs_to_this_code@Gleam.input_optional_names(()),
  fn(_) {
    let people_mappings = Gleam.Yaml.from_string("
      -
        name: John
        age: 25
      -
        name: Sarah
        age: 30
    ")

    let people = people_mappings
      |> Gleam.Dict.get("people")
      |> Gleam.List.map(
        fn
          { name, age } ->
            let greet = fn(person) { "Hello" ++ person.name }
            greet({ name: person.name, age: person.age })
        end
      )

    Gleam.io.print(people)

    Gleam.unit
  },
)

```

```
Hello John
Hello Sarah

()
```

## Wgląd w zagadnienie YAML
YAML został po raz pierwszy zaprojektowany przez Clarka Evansa w 2001 roku. Alternatywami dla YAML są między innymi formaty JSON i XML. W języku Gleam korzystamy z modułu `Gleam.Yaml` do pracy z plikami YAML.

## Zobacz również
- Dokumentacja modułu Gleam.Yaml: https://gleam.run/modules/gleam_yaml.html
- Oficjalna strona YAML: https://yaml.org/