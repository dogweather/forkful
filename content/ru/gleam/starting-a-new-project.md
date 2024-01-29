---
title:                "Начало нового проекта"
date:                  2024-01-29T00:03:12.965932-07:00
model:                 gpt-4-0125-preview
simple_title:         "Начало нового проекта"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/starting-a-new-project.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Начало нового проекта означает инициализацию новой кодовой базы с необходимой структурой и конфигурацией. Программисты делают это, чтобы начать разработку с чистого листа, обеспечивая лучшие практики и организацию с самого начала.

## Как это сделать:

Чтобы создать новый проект Gleam, вам понадобится инструмент командной строки `gleam`. Установите его, а затем выполните:

```shell
gleam new my_cool_project
```

Это создаст новый каталог с именем `my_cool_project` со следующей базовой структурой проекта:

```plaintext
my_cool_project/
├── gleam.toml
├── src
│   └── my_cool_project.gleam
└── test
    └── my_cool_project_test.gleam
```

Каталог `src` содержит основной модуль Gleam, а каталог `test` - это место, где будут находиться ваши тесты. Давайте взглянем на стандартный `my_cool_project.gleam`:

```gleam
pub fn hello_world() {
  "Hello, world!"
}
```

Просто и со вкусом. Теперь вы начали новый проект на Gleam!

## Глубокое Погружение

Gleam появился на сцене примерно в 2018 году, стремясь принести строгую статическую типизацию в экосистему Erlang — не теряя при этом её известную надёжность и модель конкуренции.

Альтернативы началу проекта с использованием `gleam new` могут включать клонирование шаблона из репозитория или вручную создание структуры файлов. Однако использование инструмента Gleam обеспечивает единообразную отправную точку и разработано так, чтобы без проблем функционировать внутри экосистемы.

За кулисами `gleam new` настраивает проект `rebar3` или `mix` в зависимости от вашего предпочтения (по умолчанию используется `rebar3`). Он заполняет необходимые файлы конфигурации, такие как `gleam.toml` для управления зависимостями и настройками проекта, и `rebar.config` или `mix.exs` для взаимодействия с Erlang или Elixir соответственно.

## Смотрите также

- Официальное руководство по началу работы с Gleam: [https://gleam.run/book/getting-started/](https://gleam.run/book/getting-started/)
- GitHub репозиторий Gleam: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Документация стандартной библиотеки Gleam: [https://hexdocs.pm/gleam_stdlib/](https://hexdocs.pm/gleam_stdlib/)
