---
title:                "Читання текстового файлу"
aliases:
- /uk/elixir/reading-a-text-file.md
date:                  2024-01-20T17:54:13.249304-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і Чому?
Читання текстового файлу — це процес отримання даних із файлу на диску. Програмісти роблять це, щоб обробляти інформацію, налаштувати програму, чи завантажити конфігурації.

## Як це зробити:
Elixir використовує модуль `File` для читання файлів. Ось базовий приклад:

```elixir
# Прочитаємо весь файл разом
{:ok, data} = File.read("filename.txt")
IO.puts(data)

# Читаємо файл по рядках
File.stream!("filename.txt")
|> Enum.each(&IO.puts(&1))
```

Припустимо, у файлі `filename.txt` є такий вміст:
```
Привіт, світ!
Це текстовий файл.
```

Вихід:
```
Привіт, світ!
Це текстовий файл.
```

## Глибше занурення
У Еліксирі, читання файлу, це взаємодія з операційною системою через BEAM (Erlang Virtual Machine). Історично, модулі для роботи з файлами у Ерланг були оптимізовані під високу проізводительність.

Альтернативами є:
- Використання модуля `:file` із Erlang.
- Підключення зовнішніх утиліт за допомогою `System.cmd/3`.

Для великих файлів рекомендують читати по рядках, а не завантажувати весь файл в пам'ять. Еліксир використовує ліниві перечислення (`Stream`), дозволяючи ефективно працювати з великими даними.

## Дивіться також
- [Elixir School: Files](https://elixirschool.com/en/lessons/basics/collections/#files)
- [Elixir documentation for File module](https://hexdocs.pm/elixir/File.html)
- [Erlang documentation for :file module](http://erlang.org/doc/man/file.html)
