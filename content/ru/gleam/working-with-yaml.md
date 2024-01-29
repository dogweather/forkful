---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:04.429473-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
YAML, "YAML Ain't Markup Language" (YAML — это не язык разметки), представляет собой удобный для человека стандарт сериализации данных. Программисты используют его для файлов конфигурации, обмена данными между языками и потому что он более читаем, чем JSON или XML.

## Как это делать:
В настоящее время в Gleam нет встроенных парсеров или библиотек YAML, по состоянию на мой последний обновление. Обычно вы бы разбирали YAML в Gleam, опираясь на функции Erlang благодаря совместимости Gleam с экосистемой Erlang. Давайте использовать библиотеку Erlang и вызвать ее из Gleam.

Сначала добавьте библиотеку YAML для Erlang в `rebar.config`:

```erlang
{deps, [yaml]}.
```

Вот как вы можете вызвать библиотеку Erlang из Gleam:

```rust
external fn parse(String) -> Result(Tuple(tuple(atom(), String)), Nil) =
  "yaml" "decode"

pub fn main() -> Result(Tuple(tuple(atom(), String)), Nil) {
  let yaml_data = "greeting: hello"
  parse(yaml_data)
}
```

Пример вывода может выглядеть так:

```elixir
Ok(#(ok, [{greeting, "hello"}]))
```

## Подробнее
YAML был выпущен в 2001 году и часто используется там, где важна читаемость для человека. Это не всегда является стандартом для сериализации данных, также широко используются JSON и XML. Однако простота YAML делает его идеальным для файлов конфигурации или простых структур данных.

В качестве альтернативы могут выступать встроенный парсер `:yamerl` в Elixir, а в Gleam для решения аналогичных задач можно использовать JSON с библиотекой `gleam/json`. Что касается реализации, то при работе с YAML в Gleam вы используете широкие возможности экосистемы BEAM — именно эта взаимодействие позволяет разбирать YAML без необходимости специализированной библиотеки Gleam.

## Смотрите также
- Спецификация YAML: https://yaml.org/spec/1.2/spec.html
- Библиотека `yaml` для Erlang: https://hex.pm/packages/yaml
- Документация библиотеки JSON для Gleam: https://hexdocs.pm/gleam_stdlib/gleam/json/
