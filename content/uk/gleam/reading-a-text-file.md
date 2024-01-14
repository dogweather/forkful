---
title:                "Gleam: Читання текстового файлу"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Чому варто читати текстовий файл? Наприклад, ви може бажати отримати дані з файлу для подальшої обробки, або отримати статистику стосовно вмісту тексту.

## Як

Зчитування текстового файлу в Gleam може бути досить простим завданням, якщо ви знаєте правильний спосіб це зробити. Будь ласка, перегляньте код нижче для прикладу:

```Gleam
import gleam/io

let file = io.file.open("my_file.txt") // відкриваємо файл для читання

let contents = io.file.read_all(file) // зчитуємо вміст файлу
io.file.close(file) // закриваємо файл

io.format("Вміст файлу: {}", [contents]) // друкуємо вміст файлу на екран

```

Вивід на екрані виглядатиме так:

```
Вміст файлу: Привіт, це мій перший текстовий файл!
```

## Глибокий погляд

Для зчитування більших файлів може бути корисним використання ітераторів у Gleam. Наприклад, ми можемо використати ітератор `file.lines()` для зчитування кожного рядка тексту окремо. Перегляньте приклад нижче:

```Gleam
import gleam/io

let file = io.file.open("my_file.txt") // відкриваємо файл для читання

let lines = io.file.lines(file) // отримуємо ітератор рядків файлу

for line in lines do // проходимося по кожному рядку і друкуємо його на екран
  io.format("Рядок: {}", [line])
end

io.file.close(file) // закриваємо файл
```

Вивід на екрані буде наступним:

```
Рядок: Привіт, це мій перший текстовий файл!
```

## Дивіться також

- [Документація Gleam про роботу з файлами](https://gleam.run/books/standard-library/files.html)
- [Приклади роботи з файлами у Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/files.gleam)