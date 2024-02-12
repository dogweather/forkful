---
title:                "Робота з YAML"
aliases:
- /uk/elixir/working-with-yaml/
date:                  2024-02-03T19:25:24.326879-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

YAML, скорочено від "YAML Ain't Markup Language" (YAML - це не мова розмітки), є стандартом серіалізації даних, зрозумілим для людини, який часто використовується для файлів конфігурації та обміну даними між мовами з різними структурами даних. Програмісти використовують його через його простоту та здатність легко представляти складні ієрархічні дані.

## Як це зробити:

Elixir не містить вбудованої підтримки YAML. Однак, ви можете використовувати сторонні бібліотеки, такі як `yamerl` або `yaml_elixir`, для роботи з YAML. Тут ми зосередимося на `yaml_elixir` через його простоту використання та комплексні можливості.

Спочатку додайте `yaml_elixir` до вашої залежності mix.exs:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Потім, запустіть `mix deps.get`, щоб отримати нову залежність.

### Читання YAML

Уявімо простий YAML файл, `config.yaml`, який виглядає так:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

Ви можете прочитати цей YAML файл і перетворити його в карту Elixir таким чином:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# Приклад використання
Config.read()
# Вивід: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### Запис YAML

Щоб записати карту назад в YAML файл:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# Приклад використання
ConfigWriter.write()
# Це створить або перезапише `new_config.yaml` з вказаним змістом
```

Зверніть увагу, як `yaml_elixir` дозволяє легко перекладати файли YAML та структури даних Elixir, роблячи його відмінним вибором для програмістів Elixir, яким необхідно працювати з даними YAML.
