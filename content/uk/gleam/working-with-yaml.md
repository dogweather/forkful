---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це & Навіщо?
YAML – це формат для зберігання і передачі даних, який легко читається людиною. Програмісти використовують його для конфігурацій файлів, оскільки він простий і зрозумілий.

## Як робити:
Gleam має обмежену підтримку роботи з YAML, так що вам може знадобитися використовувати власний парсер або рішення на іншій мові, якщо така необхідність виникне. Нижче приклад коду на Erlang, який часто використовується у сценаріях з Gleam, щоб працювати з YAML файлами.

```Gleam
// Gleam currently doesn't have a YAML library.
// You can use an Erlang library for YAML processing.
// Add `yamerl` as a dependency in your `rebar.config`.

external type YamlDocuments

fn parse_yaml(yaml_string: String) -> Result(YamlDocuments, Nil) {
  erlang.apply(atom("yamerl"), "load", [yaml_string])
}

fn main() {
  let yaml_data = "
  name: Gleam
  version: '1.0.0'
  dependencies:
    - name: yaml_lib
      version: '1.2.3'
  "

  let result = parse_yaml(yaml_data)
  case result {
    Ok(docs) -> docs
    Error(_) -> "Failed to parse YAML"
  }
}
```

## Поглиблений розбір
YAML (YAML Ain't Markup Language) з'явився у 2001 році як зручний формат для конфігураційних файлів. Від JSON відрізняється браком дужок і ком, і кращою читабельністю. Альтернативи YAML – це JSON та TOML. Програма на Gleam для роботи з YAML може включати в себе використання Erlang бібліотеки через FFI (Foreign Function Interface), тому що нативної бібліотеки під Gleam поки немає.

## Див. також
- YAML офіційний сайт: [https://yaml.org/](https://yaml.org/)
- Yamerl, Erlang бібліотека для YAML: [https://github.com/yakaz/yamerl](https://github.com/yakaz/yamerl)
- TOML у Gleam за допомогою Erlang бібліотеки: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
