---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:24.326879-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Elixir \u043D\u0435 \u043C\u0456\u0441\u0442\u0438\u0442\u044C \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\
  \u043A\u0438 YAML. \u041E\u0434\u043D\u0430\u043A, \u0432\u0438 \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0432\u0430\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\u0456\
  \ \u044F\u043A `yamerl` \u0430\u0431\u043E `yaml_elixir`, \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:48.757751-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u043D\u0435 \u043C\u0456\u0441\u0442\u0438\u0442\u044C \u0432\u0431\
  \u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\
  \u043C\u043A\u0438 YAML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

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
