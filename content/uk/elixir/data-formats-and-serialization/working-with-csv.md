---
title:                "Робота з CSV"
date:                  2024-02-03T19:20:02.556780-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Робота з файлами CSV (Comma-Separated Values, значення, розділені комами) передбачає читання з цих файлів та запис даних у них, що є поширеною потребою для завдань, пов'язаних з імпортом/експортом даних або простими рішеннями для зберігання. Програмісти використовують цю функціональність для обміну даними між системами, швидкого редагування даних або в ситуаціях, коли перевагою є легкий і легко маніпулюваний формат даних.

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
