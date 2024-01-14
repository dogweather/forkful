---
title:                "Elixir: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому
Існує багато форматів файлів конфігурації, але YAML - один з найпростіших і зрозуміліших. Працюючи з Elixir, використання YAML допоможе вам управляти конфігурацією вашого додатку та забезпечити більш зрозумілий та структурований код.

## Як
Для початку роботи з YAML у Elixir, вам необхідно встановити пакет `:yaml` через менеджер пакетів. Після цього, ви можете імпортувати модуль `YAML` та використовувати його функції, як показано нижче:

```Elixir
iex> import YAML
iex> data = %{name: "John", age: 30}
%{age: 30, name: "John"}
iex> YAML.encode(data)
"---
age: 30
name: John
"
```

Ви можете використовувати функцію `encode` для перетворення даних у строку YAML для збереження у файлі. Для читання даних з файлу, ви можете використовувати функцію `decode`:

```Elixir
iex> file = File.read!("config.yml")
"---
database:
  host: 127.0.0.1
  port: 3306
  username: root
  password: secret
"
iex> YAML.decode(file)
%{
  database: %{
    host: "127.0.0.1",
    password: "secret",
    port: 3306,
    username: "root"
  }
}
```

## Детальний огляд
YAML є легким та читабельним форматом для створення конфігураційних файлів. Він дозволяє вам представляти дані у структурованому форматі за допомогою інтуїтивно зрозумілих відступів та ключів. Крім того, YAML підтримує вкладеність, що дозволяє створювати складні структури даних.

Найбільшою перевагою використання YAML у Elixir є можливість зберігати дані у форматі `map`, який є стандартною структурою даних у Elixir. Це дозволяє легко працювати з конфігурацією та використовувати її у вашому коді.

## Дивись також
- [Офіційний сайт YAML](https://yaml.org/)
- [Elixir пакет для роботи з YAML](https://hex.pm/packages/yaml)
- [Приклади використання YAML у Elixir](https://github.com/yrashk/elixir-yaml)