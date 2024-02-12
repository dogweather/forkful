---
title:                "Робота з TOML"
aliases:
- /uk/elixir/working-with-toml.md
date:                  2024-01-26T04:21:18.105676-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і Чому?
Робота з TOML означає парсинг і генерацію даних TOML (Tom's Obvious, Minimal Language) за допомогою Elixir. Програмісти використовують його для роботи з файлами конфігурації, оскільки TOML є зрозумілим, легко парситься та добре відображається у хеш-структуру даних.

## Як це зробити:
Спочатку додайте парсер TOML до залежностей вашого mix. У цьому прикладі використовується `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Читання файлу TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Для конвертації даних Elixir в TOML:

```elixir
data = %{title: "Томл-приклад", owner: %{name: "Том Престон-Вернер"}}
toml_string = TomlElixir.encode(data)
```

Приклад виводу:

```elixir
"title = \"Томл-приклад\"\n\n[owner]\nname = \"Том Престон-Вернер\"\n"
```

## Поглиблений огляд
TOML був створений Томом Престон-Вернером, співзасновником GitHub, для використання у файлах конфігурації. Його створено бути простішим за XML та більш лаконічним за YAML, при цьому зберігаючи послідовність.

Альтернативи включають файли JSON, YAML та INI, кожен із яких має свої компроміси у легкості читання та сумісності структури даних. TOML перевершує в чіткому представленні табличних даних і вкладеному групуванні даних.

У Elixir обробка TOML залежить від бібліотек декодування та кодування, які перетворюють рядки TOML на мапи Elixir і навпаки. Парсинг працює шляхом співставлення синтаксичних правил TOML і конвертації їх у типи даних Elixir. Кодування робить протилежне, відображаючи типи даних Elixir назад до дійсного синтаксису TOML.

## Дивіться також
- Мова TOML: https://toml.io/en/
- Репозиторій `toml-elixir` на GitHub: https://github.com/bitwalker/toml-elixir
- Деталі HEX-пакету для `toml-elixir`: https://hex.pm/packages/toml_elixir
