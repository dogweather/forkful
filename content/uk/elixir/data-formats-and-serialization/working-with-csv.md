---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:02.556780-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV (Comma-Separated Values, \u0437\u043D\u0430\u0447\u0435\u043D\
  \u043D\u044F, \u0440\u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\
  \u043C\u0430\u043C\u0438) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\
  \u0454 \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0437 \u0446\u0438\u0445 \u0444\
  \u0430\u0439\u043B\u0456\u0432 \u0442\u0430 \u0437\u0430\u043F\u0438\u0441 \u0434\
  \u0430\u043D\u0438\u0445 \u0443 \u043D\u0438\u0445, \u0449\u043E \u0454 \u043F\u043E\
  \u0448\u0438\u0440\u0435\u043D\u043E\u044E \u043F\u043E\u0442\u0440\u0435\u0431\u043E\
  \u044E\u2026"
lastmod: '2024-03-13T22:44:48.761245-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV (Comma-Separated Values, \u0437\u043D\u0430\u0447\u0435\u043D\u043D\
  \u044F, \u0440\u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\
  \u0430\u043C\u0438) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454\
  \ \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0437 \u0446\u0438\u0445 \u0444\u0430\
  \u0439\u043B\u0456\u0432 \u0442\u0430 \u0437\u0430\u043F\u0438\u0441 \u0434\u0430\
  \u043D\u0438\u0445 \u0443 \u043D\u0438\u0445, \u0449\u043E \u0454 \u043F\u043E\u0448\
  \u0438\u0440\u0435\u043D\u043E\u044E \u043F\u043E\u0442\u0440\u0435\u0431\u043E\u044E\
  \ \u0434\u043B\u044F \u0437\u0430\u0432\u0434\u0430\u043D\u044C, \u043F\u043E\u0432\
  '\u044F\u0437\u0430\u043D\u0438\u0445 \u0437 \u0456\u043C\u043F\u043E\u0440\u0442\
  \u043E\u043C/\u0435\u043A\u0441\u043F\u043E\u0440\u0442\u043E\u043C \u0434\u0430\
  \u043D\u0438\u0445 \u0430\u0431\u043E \u043F\u0440\u043E\u0441\u0442\u0438\u043C\
  \u0438 \u0440\u0456\u0448\u0435\u043D\u043D\u044F\u043C\u0438 \u0434\u043B\u044F\
  \ \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

## Як це зробити:
Elixir, з його потужною відповідністю шаблонів і підтримкою каналізації, може ефективно працювати з файлами CSV, навіть без сторонніх бібліотек. Однак, для більш складних потреб, бібліотека `nimble_csv` є швидким та простим вибором.

### Читання файлу CSV без зовнішніх бібліотек
Ви можете прочитати та розібрати файл CSV, використовуючи вбудовані функції Elixir:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Приклад використання
CSVReader.read_file("data.csv")
# Вивід: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### Запис у файл CSV
Аналогічно, для запису даних у файл CSV:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# Приклад використання
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# Створює output.csv з даними у форматі CSV
```

### Використання `nimble_csv`
Для більш складної роботи з даними CSV, `nimble_csv` надає потужний та гнучкий спосіб роботи з даними CSV. Спочатку додайте `nimble_csv` до ваших залежностей у `mix.exs` і виконайте `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Розбір даних CSV з `nimble_csv`:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Приклад використання
MyCSVParser.parse("data.csv")
# Вивід з nimble_csv можна налаштувати в залежності від означення, але загалом виглядає як список списків або кортежів залежно від того, як ви налаштували свій розбірник.
```

Запис даних CSV за допомогою `nimble_csv` вимагає ручного перетворення ваших даних у належний формат, а потім запису їх у файл, схоже на приклад з чистим Elixir, але використовуючи `nimble_csv` для генерування правильно форматованих рядків CSV.

Вибравши відповідний підхід до складності вашого завдання, ви можете обробляти файли CSV в Elixir з великою гнучкістю та потужністю.
