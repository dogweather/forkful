---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Читання текстових файлів - це процес отримання даних з файлу в текстовому форматі. Програмісти використовують такий підхід для роботи з даними, збереженими в файлі.

## Як це зробити:

Використайте наступний код для читання текстового файлу в Gleam:

```Gleam
import gleam/otp/process
import gleam/io.{Io, Writer, Reader, Output, Input}

fn read_to_string(path: String) -> Result(String, Nil) {
  let file_input = gleam@fs@read!(path)
  let Io{reader} = Input.from(file_input)

  Ok(Reader.to_string(reader))
}

fn main() -> Nil {
  let content = read_to_string("/path/to/your/textfile.txt")
  case content {
    Ok(content_str) ->
      process.println(content_str)
    Error(_) ->
      process.println("Error reading the file!")
  }
}
```
Переконайтесь, що шлях до файлу ```"/path/to/your/textfile.txt"``` вказує на існуючий текстовий файл.

## Попереду:

1. **Історичний контекст**: Текстові файли були серед перших форматів даних, розроблений IBM у 1960-х, і досі активно використовуються через свою простоту і читабельність.

2. **Альтернативи**: В залежності від завдання, може бути зручніше працювати з форматами JSON, XML, CSV або бінарними форматами, як-от Protobuf.

3. **Деталі реалізації**: Gleam використовує свій модуль ```gleam/otp/process``` для введення/виведення потоків. Це дозволяє робити зчитування текстового файла асинхронним.

## Додатково:
