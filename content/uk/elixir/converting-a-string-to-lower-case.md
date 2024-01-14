---
title:                "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому
 Конвертація рядка в нижній регістр можна бути корисною, коли ви хочете порівняти рядки без урахування регістру, наприклад у функції пошуку або сортуванні. Також це допоможе уникнути проблем з порівнянням рядків різного регістру.

 ## Як
 ```
 # Код для конвертації рядка в нижній регістр
 string = "Привіт, Світ!"
 IO.puts String.downcase(string)
 # Виводить "привіт, світ!"
```

## Глибоке дослідження
Функція String.downcase(string) використовує вбудовану функцію Erlang, яка перетворює рядок у список кодів символів Unicode. Потім цей список кодів проганяється через функцію Unicode.downcase(chars), яка конвертує кожен символ у нижній регістр. Нарешті, отриманий список рядків перетворюється назад у рядок і повертається як результат. Цей процес є ефективним та дозволяє обробляти рядки будь-якої довжини.

## Дивись також
 - [Документація Elixir для String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
 - [Документація Erlang для Unicode.downcase/1](https://erlang.org/doc/man/unicode.html#downcase-1)