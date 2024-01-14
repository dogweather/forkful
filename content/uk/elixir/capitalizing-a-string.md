---
title:                "Elixir: Капіталізація рядка"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Нерідко у програмуванні потрібно змінити частину тексту, наприклад, зробити першу літеру слова великою. Це може бути потрібно для коректного відображення назв у списку, створення заголовків або для декоративних цілей.

## Як це зробити

Для зміни першої літери у рядку в Elixir, використовуйте функцію `String.capitalize`. Наприклад:

```Elixir
String.capitalize("elixir") # повертає "Elixir"
String.capitalize("hello world") # повертає "Hello world"
```

Ви також можете використовувати цю функцію для зміни першої літери у кожному слові у рядку, використовуючи `String.capitalize_every/2`:

```Elixir
String.capitalize_every("hello world", "_") # повертає "Hello_World"
```

## Глибше вдивляємось

Якщо ви хочете більш детально ознайомитись з `String.capitalize` та іншими функціями для роботи зі строками в Elixir, ви можете ознайомитись з офіційною документацією на сайті [Elixir lang](https://elixir-lang.org/docs.html).

## Дивіться також

- [Офіційна документація Elixir lang](https://elixir-lang.org/docs.html)
- [Стаття про роботу з строками в Elixir](https://medium.com/coding-with-clarity/strings-in-elixir-7ff9614daa41)
- [Відео уроки про Elixir на українській мові](https://www.youtube.com/watch?v=nuhfp6sbpuY)