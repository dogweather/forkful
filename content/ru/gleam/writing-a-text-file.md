---
title:                "Создание текстового файла"
date:                  2024-01-29T00:05:36.790244-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание текстового файла"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/writing-a-text-file.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Создание текстового файла означает сохранение данных в виде текста, который могут прочитать люди. Программисты делают это для сохранения результатов работы, конфигурации приложений или регистрации событий.

## Как:
Gleam предлагает работу с файлами I/O через свою стандартную библиотеку. Вот как записать в файл:
```gleam
import gleam/io
import gleam/result

pub fn write_to_file(contents: String) -> Result(Nil, IOError) {
  result.then(
    io.open("output.txt", [io.Write]),
    fn(file) { io.write(file, contents) }
  )
}

pub fn main() {
  case write_to_file("Привет, Gleam!") {
    Ok(_) -> io.print("Файл успешно записан")
    Error(err) -> io.print(err)
  }
}
```
Если успешно, ваш `output.txt` будет содержать "Привет, Gleam!".

## Подробнее
Исторически обработка файлов критически важна для долгосрочного хранения данных. Подход Gleam похож на подход Erlang, на котором он построен. Альтернативы включают системы баз данных или хранение данных в памяти для временных данных. Стандартная библиотека Gleam поддерживает минималистичный API, предпочитая явную обработку ошибок с использованием типа `Result`.

## Смотрите также
- [Модуль File Erlang](http://erlang.org/doc/man/file.html) для понимания операций низкого уровня, которые абстрагирует Gleam.
- [Запись файлов в Rust](https://doc.rust-lang.org/std/fs/struct.File.html#method.create), для сравнения с другим системным языком.
