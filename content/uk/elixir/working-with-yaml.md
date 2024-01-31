---
title:                "Робота з YAML"
date:                  2024-01-19
simple_title:         "Робота з YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це та Навіщо?
YAML – це формат для серіалізації даних, любимий за легкість читання. Програмісти використовують його для конфігурації проектів, документації та обміну даними між різними мовами програмування.

## Як робити:
Щоб працювати з YAML в Elixir, використовуємо бібліотеку `YamlElixir`. Спочатку встановлюємо:

```elixir
# Додаємо в mix.exs
defp deps do
  [
    {:yaml_elixir, "~> 2.5"}
  ]
end
```
Парсимо YAML файл:
```elixir
# Приклад парсингу YAML файлу
{:ok, yaml} = YamlElixir.read_from_file("config.yaml")
IO.inspect(yaml)
```
Генеруємо YAML з Elixir структур:
```elixir
# В YAML з Elixir map
map = %{name: "Oleh", age: 28}
yaml_content = YamlElixir.write_to_string(map)
IO.puts(yaml_content)
```
Вивід:
```yaml
---
age: 28
name: Oleh
```

## Поглиблений огляд:
YAML (YAML Ain't Markup Language) з'явився у 2001 році як зручний формат для серіалізації даних. В Elixir, зазвичай, працюють з YAML через зовнішні бібліотеки, оскільки стандартна бібліотека цього не підтримує. Альтернативою YAML є JSON і XML, які також широко використовуються для подібних цілей, але YAML виграш у читабельності. Передбачається, що YAML ефективніше для людського сприйняття, а JSON – для машинного. В Elixir працювати з YAML легко та ефективно завдяки бібліотекам, які хендлять усі нюанси парсингу та генерації.

## Дивіться також:
- Документація `YamlElixir`: [https://hexdocs.pm/yaml_elixir](https://hexdocs.pm/yaml_elixir)
- YAML офіційний сайт: [https://yaml.org](https://yaml.org)
- Серіалізація даних в Elixir: [https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
