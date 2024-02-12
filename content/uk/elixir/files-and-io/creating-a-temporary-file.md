---
title:                "Створення тимчасового файлу"
aliases:
- /uk/elixir/creating-a-temporary-file.md
date:                  2024-01-20T17:39:58.837679-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
(Що та Чому?)
В Elixir створюють тимчасові файли для безпечного збереження даних, які не потрібні після завершення програми. Це корисно для тестування, маніпуляції даними чи як місце для тимчасового зберігання.

## How to:
(Як це зробити:)
```elixir
# Підключення модулю для роботи з файлами
File.mkdir_p("tmp")

# Створення тимчасового файла
{:ok, file} = File.open("tmp/my_temp_file.txt", [:write, :exclusive])

# Запис у файл
IO.write(file, "Some temporary data")

# Закриття файла
File.close(file)

# Вивід вмісту тимчасового файла
IO.puts(File.read!("tmp/my_temp_file.txt"))
```

Вивід буде такий:
```
Some temporary data
```

## Deep Dive
(Поглиблений огляд)
Elixir використовує систему файлів операційної системи для створення і використання тимчасових файлів. Історично, тимчасові файли були необхідні для покращення продуктивності і зниження навантаження на пам'ять. Сьогодні їх також застосовують для забезпечення конфіденційності, оскільки вони видаляються після використання. Існує декілька альтернативних підходів, як от `:os.tmpdir` для отримання шляху системної тимчасової папки або використання пакетів сторонніх розробників як `Temp` для більш гнучкого управління тимчасовими файлами.

## See Also
(Дивіться також)
- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Erlang :os module documentation](http://erlang.org/doc/man/os.html)
