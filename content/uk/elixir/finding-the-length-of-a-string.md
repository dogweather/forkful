---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Визначення довжини рядка в Elixir

## Що і Навіщо?

Визначення довжини рядка - це процес з'ясування кількості символів у даному рядку. Програмісти роблять це, щоб контролювати та керувати обробкою текстових даних у ваших програмах.

## Як це зробити:

У Elixir, функція `String.length/1` використовується для визначення довжини рядка. Подивімося на декілька прикладів:

```Elixir
name = "Володимир"
IO.puts(String.length(name))
# Виведе: 9

sentence = "Це приклад речення в Elixir"
IO.puts(String.length(sentence))
# Виведе: 27
```

## Занурення в деталі

1. Історичний контекст: Elixir виник на основі Erlang VM, який був створений в 1986 році для розробки надійних систем. Функція визначення довжини рядка була включена з самого початку, щоб обробляти текстові дані.

2. Альтернативи: Можна також обчислити довжину рядка за допомогою функції `byte_size/1`. Але вона повертає довжину в байтах, а не в символах.

```Elixir
IO.puts(byte_size("Привіт"))
# Виведе: 12, а не 6
```

3. Деталі реалізації: В Elixir, рядки - це бінарні дані UTF-8. Функція `String.length/1` сканує рядок і рахує кількість символів UTF-8.
   
## Дивіться також

1. [Elixir Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html)
2. [Elixir School on Strings](https://uk.elixirschool.com/lessons/basics/strings) 
3. [How Does the Length Function work?](https://stackoverflow.com/questions/4859217/how-does-the-length-function-work-in-erlang)