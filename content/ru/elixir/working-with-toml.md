---
title:                "Работа с TOML"
date:                  2024-01-29T00:04:41.047777-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с TOML включает в себя разбор и создание данных в формате TOML (Tom's Obvious, Minimal Language - Простой и Минимальный Язык Тома) с использованием Elixir. Программисты используют его для управления конфигурационными файлами, потому что TOML легко читается, легко разбирается и хорошо соответствует структуре данных типа хеш.

## Как это сделать:
Сначала добавьте парсер TOML в зависимости своего проекта mix. В этом примере используется `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Чтение файла TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Для преобразования данных Elixir в TOML:

```elixir
data = %{title: "Пример TOML", owner: %{name: "Том Престон-Вернер"}}
toml_string = TomlElixir.encode(data)
```

Пример вывода:

```elixir
"title = \"Пример TOML\"\n\n[owner]\nname = \"Том Престон-Вернер\"\n"
```

## Подробный анализ
TOML был создан Томом Престон-Вернером, сооснователем GitHub, для использования в конфигурационных файлах. Он разработан, чтобы быть более простым, чем XML и более кратким, чем YAML, сохраняя при этом последовательность.

Альтернативы включают файлы JSON, YAML и INI, каждый из которых имеет свои компромиссы в понятности для человека и совместимости структуры данных. TOML преуспевает в четком представлении табличных данных и вложенной группировке данных.

В Elixir работа с TOML зависит от библиотек кодирования и декодирования, которые преобразуют строки TOML в карты Elixir и обратно. Разбор работает путем сопоставления с правилами синтаксиса TOML и их преобразования в типы данных Elixir. Кодирование делает обратное, сопоставляя типы данных Elixir с допустимым синтаксисом TOML.

## Смотрите также
- Язык TOML: https://toml.io/en/
- репозиторий `toml-elixir` на GitHub: https://github.com/bitwalker/toml-elixir
- детали пакета Hex для `toml-elixir`: https://hex.pm/packages/toml_elixir
