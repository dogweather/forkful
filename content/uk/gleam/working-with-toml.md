---
title:                "Робота з TOML"
date:                  2024-01-26T04:22:41.894101-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з TOML означає парсинг і генерацію файлів TOML (Tom's Obvious, Minimal Language — очевидний мінімалістичний мова Тома) за допомогою коду. Програмісти використовують TOML для легкочитаних файлів конфігурації та серіалізації даних завдяки його зрозумілій семантиці та сумісності зі стандартними типами даних.

## Як:
Gleam не має вбудованої підтримки TOML, тому вам знадобиться зовнішня бібліотека. Наприклад:

```gleam
// Припускаючи, що у вас є бібліотека для парсингу TOML:
import toml/{Parser, Encoder}

// Парсинг вмісту TOML
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Використання розпарсених даних
match parsed {
  Ok(data) -> "Дані успішно розпарсені!"
  Error(_) -> "Не вдалося розпарсити дані."
}

// Генерація вмісту TOML зі структури даних Gleam
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Приклад виводу:

```
Дані успішно розпарсені!
```

## Поглиблене вивчення
TOML було випущено в 2013 році Томом Престон-Вернером. Його мета: бути більш зрозумілим і прямолінійним, ніж XML, і менш складним, ніж YAML для конфігурацій файлів. Незважаючи на простоту, він надійний для структурованих даних, пропонуючи явний та легко зрозумілий синтаксис. Альтернативи включають JSON, YAML та INI, але мінімалістичний і чіткий синтаксис TOML часто переважає для файлів конфігурації. Реалізація TOML у Gleam включає дві основні дії: парсинг TOML у нативні структури даних і серіалізація нативних структур даних у TOML. Більшість бібліотек TOML для Erlang або Elixir можуть бути використані у Gleam завдяки його сумісності з мовами BEAM, забезпечуючи безшовну інтеграцію в проєктах Gleam.

## Дивіться також
- Специфікації мови TOML: [https://toml.io/en/](https://toml.io/en/)
- Парсер TOML для Erlang: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML на GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)