---
title:                "Чтение текстового файла"
date:                  2024-01-29T00:00:57.014054-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Чтение текстового файла означает получение данных из файла, состоящего из текста на вашем диске. Программисты делают это для доступа и манипулирования хранимой информацией, например, конфигурациями, журналами или любыми данными, которые требуются их приложениям.

## Как:
Gleam не включает в себя работу с файлами в свою стандартную библиотеку, поэтому мы будем использовать функции Erlang. Начнем с открытия текстового файла с помощью `file.open/2`, прочитаем его содержимое, обработаем возможные ошибки и, наконец, закроем файл. Вот как это делается:

```gleam
import gleam/erlang
import gleam/result

fn main() {
  case erlang.file.open("example.txt", [read]) {
    Ok(file) ->
      case erlang.file.read(file) {
        Ok(data) -> {
          erlang.io.format("Content: ~p~n", [data])
          erlang.file.close(file)
        }
        Error(err) -> {
          erlang.io.format("Error reading file: ~p~n", [err])
        }
      }
    Error(err) ->
      erlang.io.format("Error opening file: ~p~n", [err])
  }
}
```

Запустите это, и вы увидите содержимое вашего текстового файла, или ошибку, если что-то пойдет не так.

## Подробнее
Чтение файлов - это ничего нового; это делается в программировании с дней перфокарт. Gleam, язык со статической типизацией, который компилируется в виртуальную машину Erlang, опирается на зрелую экосистему Erlang для операций с файлами. У вас также есть другие варианты: асинхронное чтение, потоковая передача строк или использование библиотеки вроде `gleam_otp` для более Gleam-ообразного подхода.

Понимание работы с файлами включает в себя обработку ошибок. Файлы могут не существовать, быть заблокированы или у вас может не быть разрешения. Сопоставление по образцу и модуль `result` в Gleam предоставляют вам чистый путь для управления неожиданностями.

Наконец, учитывайте размер вашего файла. Наш простой `erlang.file.read` считывает его целиком в память, что может быть проблематично для огромных файлов. Потоковое чтение частей или строк было бы более эффективным.

## Смотрите также
- [Документация модуля файла Erlang](http://erlang.org/doc/man/file.html), поскольку мы используем возможности Erlang.
- [Документация IO Erlang](http://erlang.org/doc/apps/stdlib/io_protocol.html) для понимания, как работает ввод/вывод за кулисами.