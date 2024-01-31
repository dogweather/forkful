---
title:                "Робота з CSV файлами"
date:                  2024-01-19
simple_title:         "Робота з CSV файлами"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV - це формат даних, в якому цінності відокремлені комами. Програмісти використовують його для обробки і обміну табличними даними через його простоту і універсальність.

## How to:
```elixir
# Інсталювання CSV бібліотеки NimbleCSV
defp deps do
  [
    {:nimble_csv, "~> 1.1"}
  ]
end

# Парсинг CSV рядка
csv_data = "name,age\John,28\rJane,31"
{data, _rest} = NimbleCSV.RFC4180.parse_string csv_data, separator: ?, 
IO.inspect data

# Запис CSV даних у файл
list_of_lists = [["name", "age"], ["John", 28], ["Jane", 31]]
:ok = NimbleCSV.RFC4180.write_file "people.csv", list_of_lists
```
Вивід:
```
[["name", "age"], ["John", "28"], ["Jane", "31"]]
```

## Deep Dive
NimbleCSV - це бібліотека Elixir для обробки CSV, що створена Жозе Валімом. Існують інші опції, як CSV operations in Elixir core, але NimbleCSV більш гнучкий і швидкий. Реалізує макроси для створення власних парсерів та забезпечує ефективну обробку великих файлів.

## See Also
- [NimbleCSV Documentation](https://hexdocs.pm/nimble_csv/NimbleCSV.html)
- [RFC 4180, the CSV standard](https://tools.ietf.org/html/rfc4180)
- [CSV (comma-separated values) в Elixir](https://elixir-lang.org/getting-started/mix-otp/agent.html#agents) – як працювати з CSV без зовнішніх бібліотек.
