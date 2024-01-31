---
title:                "Починаємо новий проект"
date:                  2024-01-20T18:03:38.797099-07:00
model:                 gpt-4-1106-preview
simple_title:         "Починаємо новий проект"

category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що та Чому?
Починати новий проєкт у Elixir - це всього лиш створення нового середовища для ваших майбутніх рішень. Програмісти роблять це, щоб організувати код і зробити його легше підтримувати.

## Як робити:
```Elixir
# Встановлюємо Mix, інструмент для створення, компілювання та управління проєктами
mix new my_project

# Перехід до директорії проєкту
cd my_project

# Запускаємо тести (на початковому етапі проєкту вони повинні пройти успішно)
mix test
```
Приклад виводу:
```plaintext
Compiling 1 file (.ex)
Generated my_project app
......

Finished in 0.03 seconds
3 tests, 0 failures
```

## Поглиблений Розгляд:
Elixir базується на Erlang VM, що забезпечує надійність та паралельність процесів. У 2011 році Хосе Валім створив Elixir, спрощуючи створення програм з високою доступністю. Мікс є основним інструментом проєктування у Elixir, забезпечуючи зручне управління залежностями та задачами. 

Існують альтернативи, як-от Rebar3 для Erlang, але Mix втілив у собі практичність і вдосконалення для Elixir екосистеми. Стартовий процес проєкту з Mix включає створення каркаса з bin-скриптами, модулями та тестами, закладаючи основу під майбутній код та сприяння впровадженню TDD (Test-Driven Development).

## Дивіться також:
- [Офіційна документація Elixir](https://elixir-lang.org/docs.html)
- [Огляд Mix у документації Elixir](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Hex.pm](https://hex.pm/) - менеджер пакетів для Elixir, для пошуку та включення залежностей.
