---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:55.517412-07:00
description: "\u041A\u0430\u043A: \u0427\u0442\u043E\u0431\u044B \u0441\u043E\u0437\
  \u0434\u0430\u0442\u044C \u043D\u043E\u0432\u044B\u0439 \u043F\u0440\u043E\u0435\
  \u043A\u0442, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u0443 `mix new`."
lastmod: '2024-03-13T22:44:44.429298-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0441\u043E\u0437\u0434\u0430\u0442\u044C\
  \ \u043D\u043E\u0432\u044B\u0439 \u043F\u0440\u043E\u0435\u043A\u0442, \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 \u043A\u043E\u043C\u0430\u043D\
  \u0434\u0443 `mix new`."
title: "\u041D\u0430\u0447\u0430\u043B\u043E \u043D\u043E\u0432\u043E\u0433\u043E\
  \ \u043F\u0440\u043E\u0435\u043A\u0442\u0430"
weight: 1
---

## Как:
Чтобы создать новый проект, используйте команду `mix new`:

```elixir
$ mix new my_app
```

Вы увидите что-то вроде этого:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_app.ex
* creating test
* creating test/test_helper.exs
* creating test/my_app_test.exs
```

Перейдите в директорию вашего нового проекта:

```elixir
$ cd my_app
```

Теперь вы можете запустить ваш проект или его тесты:

Запустите ваш проект:

```elixir
$ iex -S mix
```
Протестируйте его:

```elixir
$ mix test
```

## Глубокое Погружение
Инструмент сборки Elixir, Mix, появился из желания предоставить надежный и унифицированный способ создания, настройки и управления проектами. Он был вдохновлен инструментами из других экосистем, такими как Bundler и Rake из Ruby. Mix вносит управление зависимостями и автоматизацию задач в инструментарий Elixir. Его альтернативы в других языках могут быть npm для Node.js или Maven для Java. Однако Mix адаптирован для среды выполнения Elixir и интегрирован с его идиоматическими паттернами. Команда `mix new` создает стандартную структуру с предопределенными директориями и файлами, такими как файлы конфигурации, определения модулей и наборы тестов. Следование конвенциям ключевое в Elixir; это способствует согласованности кода и его читаемости в проектах на Elixir.

## Смотрите Также
- Официальная документация `mix`: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- Руководство проекта от Elixir School: [https://elixirschool.com/en/lessons/basics/mix/](https://elixirschool.com/en/lessons/basics/mix/)
