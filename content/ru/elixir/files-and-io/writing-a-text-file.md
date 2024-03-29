---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:17.604646-07:00
description: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u0430\u0439\u043B \u0432 \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0435, \u043F\u043E\u043D\u044F\u0442\u043D\u043E\
  \u043C \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u0443. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F \u0441\u043E\u0445\u0440\u0430\u043D\u0435\
  \u043D\u0438\u044F \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438\
  , \u0442\u0430\u043A\u043E\u0439 \u043A\u0430\u043A \u043B\u043E\u0433\u0438,\u2026"
lastmod: '2024-03-13T22:44:44.465179-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u0430\u0439\u043B \u0432 \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0435, \u043F\u043E\u043D\u044F\u0442\u043D\u043E\
  \u043C \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u0443. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F \u0441\u043E\u0445\u0440\u0430\u043D\u0435\
  \u043D\u0438\u044F \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438\
  , \u0442\u0430\u043A\u043E\u0439 \u043A\u0430\u043A \u043B\u043E\u0433\u0438,\u2026"
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
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
