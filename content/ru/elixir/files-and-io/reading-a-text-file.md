---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:32.300081-07:00
description: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438\u0437 \u0444\u0430\u0439\u043B\u0430\
  \ \u0432 \u0432\u0430\u0448\u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\
  \u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u0438\u043B\u0438 \u0430\u043D\
  \u0430\u043B\u0438\u0437\u0430 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\
  \u0433\u043E, \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440,\u2026"
lastmod: '2024-03-13T22:44:44.463033-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438\u0437 \u0444\u0430\u0439\u043B\u0430\
  \ \u0432 \u0432\u0430\u0448\u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\
  \u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u0438\u043B\u0438 \u0430\u043D\
  \u0430\u043B\u0438\u0437\u0430 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\
  \u0433\u043E, \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440,\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
---

{{< edit_this_page >}}

## Что и Почему?

Чтение текстового файла означает извлечение данных из файла в вашу программу. Программисты делают это для обработки или анализа содержимого, например, чтения конфигураций, анализа журналов или импорта данных.

## Как:

Вот как прочитать весь содержимое текстового файла с именем `example.txt`:

```elixir
File.read("example.txt")
```

Пример вывода, если `example.txt` содержит "Hello, Elixir!":

```elixir
{:ok, "Hello, Elixir!"}
```

Для построчного чтения файла:

```elixir
File.stream!("example.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

Это выведет каждую строку `example.txt` в консоль.

## Погружение в детали

В Elixir `File.read/1` и `File.stream!/1` являются типичными способами чтения текстовых файлов. Исторически потребность в чтении файлов в программировании возникает из необходимости хранить и извлекать данные. В раннем периоде компьютерной эры это делалось с помощью перфокарт или магнитных лент. Сегодня мы используем различные устройства хранения данных, такие как SSD, HDD и другие.

Альтернативой `File.read/1` является `File.read!/1`, которая генерирует ошибку, если что-то идет не так, вместо возвращения кортежа. Аналогично, `File.stream!/1` отличается от `File.stream/1` тем, что при сбое генерирует ошибку, а не возвращает кортеж с ошибкой.

Внутренняя реализация работает с двоичными данными. Текст преобразуется в двоичные данные Elixir, который занимается подлежащими байтами и кодированием.

## Смотрите также:

- Официальная документация модуля `File` Elixir: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
