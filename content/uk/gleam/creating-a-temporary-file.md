---
title:                "Gleam: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Генерація тимчасових файлів є необхідним елементом для багатьох програм, тому цей пост допоможе вам зрозуміти, як створювати ці файли для використання у вашому коді.

## Як створити тимчасовий файл

Існує кілька способів створення тимчасових файлів у Gleam. Один з них - використання вбудованої функції `File.temp()`, яка створює файл у вказаній директорії з унікальним іменем.

```Gleam
import gleam/file

// Створюємо тимчасовий файл у тимчасовій директорії
let temp_file = file.temp("temp_dir")

// Перевіряємо чи файл був створений
file.exists(temp_file) // true

// Видаляємо тимчасовий файл після виконання програми
file.delete(temp_file)

```

Ще одним способом є використання модуля `Tempfile` з бібліотеки `gleam/os`, який дозволяє створювати тимчасовий файл з потоком даних.

```Gleam
import gleam/os
import gleam/io/tempfile

let write_data = "Hello, world!"

// Використовуємо Tempfile модуль з gleam/os бібліотеки
Tempfile.with_file(
  // Вказуємо передати дані для запису
  write_data,
  // Тимчасовий файл буде створений у поточній директорії
  fn tempfile -> {
    // Користуємося файлом для запису даних
    gleam/os.write_file(tempfile, write_data)
    // Повертаємо дані, що були записані у файл
    gleam/os.read_file(tempfile)
  }
) // "Hello, world!"

```

## Глибоке дослідження

Використання функції `File.temp()` базується на системному виклику `mkstemp()`, який створює тимчасовий файл з унікальним іменем. Це гарантує, що імена файлів будуть унікальними і не будуть конфліктувати з іншими файлами в системі.

Також важливо врахувати, що тимчасові файли будуть видалені після закриття програми або після виклику функції `file.delete()`.

## Дивись також

- [Документація Gleam про тимчасові файли](https://gleam.run/core/file.html#temp)
- [Документація Gleam про бібліотеку gleam/os](https://gleam.run/stdlib/gleam-os.html)
- [Офіційний сайт Gleam](https://gleam.run/)