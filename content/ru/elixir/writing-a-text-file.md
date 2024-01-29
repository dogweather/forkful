---
title:                "Создание текстового файла"
date:                  2024-01-29T00:05:17.604646-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание текстового файла"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/writing-a-text-file.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Запись текстового файла включает сохранение данных в файл в формате, понятном человеку. Программисты делают это для сохранения информации, такой как логи, настройки и результаты вывода данных.

## Как это сделать:

Elixir делает запись текстовых файлов простой. Вот простой пример записи в файл с именем "hello.txt".

```elixir
File.write("hello.txt", "Привет, Мир!\n")
```
После выполнения этого, проверьте "hello.txt", и там будет написано:

```
Привет, Мир!
```
Для добавления текста вместо перезаписи:

```elixir
File.write("hello.txt", "Еще одна строка!\n", [:append])
```

Теперь в "hello.txt" будет показано:

```
Привет, Мир!
Еще одна строка!
```

## Подробнее

Подход Elixir к записи файлов отражает его наследие Erlang, сосредоточенное на надежности и параллелизме. Альтернативы включают использование потоков для больших данных. Внутренне Elixir использует модуль Erlang :file, который взаимодействует с базовой ОС.

## Смотрите также

- Документация модуля `File` Elixir: https://hexdocs.pm/elixir/File.html
- Документация модуля `:file` Erlang: https://erlang.org/doc/man/file.html 
- Узнайте о модуле Stream Elixir для обработки больших данных: https://hexdocs.pm/elixir/Stream.html
