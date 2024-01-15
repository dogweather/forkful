---
title:                "Робота з yaml"
html_title:           "Elixir: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Навіщо

Екосистема YAML є популярною серед розробників через його простоту та читабельність, що робить його ідеальним для зберігання та обміну даними. Чи ти вже працюєш з Elixir, або тільки починаєш, робота з YAML буде корисною для будь-якого проєкту.

## Як

Найпростішим способом роботи з YAML в Elixir є використання бібліотеки [YAXL](https://hex.pm/packages/yaxl). Ця бібліотека надає багато функцій для зчитування та запису даних у YAML форматі.

```Elixir
# Читаємо дані з файлу "config.yml"
{:ok, data} = YAXL.read("./config.yml")

# Змінюємо значення поля "database" та записуємо зміни у новий файл "new_config.yml"
new_data = data |> Map.put(:database, "postgresql")
:ok = YAXL.write("./new_config.yml", new_data)
```

### Вивід:

```Elixir
{:ok, %{application: "my_app", database: "postgresql"}}
```

## Детальний огляд

Якщо вам потрібно більше контролю над процесом роботи з YAML, ви можете використовувати вбудовану бібліотеку [YAML](https://hexdocs.pm/elixir/YAML.html). Ця бібліотека надає більше опцій та параметрів для зберігання та обробки даних.

```Elixir
# Читаємо дані з файлу "config.yml" та вказуємо опцію "symbolize_keys" для передачі ключів у вигляді атомів
{:ok, data} = YAML.file("./config.yml", symbolize_keys: true)

# Додамо пакет "cowboy" до поля "applications" та використаємо опцію "pretty" для більш зрозумілого форматування даних у файлі "new_config.yml"
new_data = data |> Keyword.put(:applications, [:cowboy | data[:applications]])
:ok = YAML.write("./new_config.yml", new_data, pretty: true)
```

### Вивід:

```Elixir
{:ok,
 %{application: "my_app",
   applications: [:cowboy, :logger, :elixir],
   port: 80,
   version: "1.0.0"}}
```

## Дивись також

- [YAXL на HexDocs](https://hexdocs.pm/yaxl/readme.html)
- [YAML на HexDocs](https://hexdocs.pm/elixir/YAML.html)