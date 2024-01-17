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

Що & Чому?

Працювання з YAML це про те, щоб зберігати та читати структуровані дані в Elixir. Це корисно для програмістів, оскільки вони можуть легше організувати та обробляти велику кількість інформації.

Як:

```Elixir
# Завантажити бібліотеку YAML
{:ok, yaml} = File.read("config.yml") |> YAML.decode()
# Змінити значення "count" на 2
yaml = %{yaml | count: 2}
# Зберегти зміни
yaml |> YAML.encode() |> File.write("config.yml")
```

Результат:

```yaml
count: 2
```

Глибоке занурення:

У принципі, YAML створений для людей, щоб було легше створювати та читати структуровані дані. Це було запроваджено в 2001 році та з тих пір отримало популярність у багатьох мов програмування. У Elixir також є інші засоби для роботи зі структурованими даними, такими як JSON та CSV.

Дивіться також:

- [Офіційна документація для YAML](https://yaml.org/)
- [Дані структури в Elixir](https://elixir-lang.org/getting-started/structs.html)
- [Вибір між YAML, JSON та іншими форматами для зберігання даних](https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json-when-to-prefer-one-over-the-other)