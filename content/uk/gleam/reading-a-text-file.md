---
title:    "Gleam: Читання текстового файлу"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

Зачем:  Лише 1-2 речення, пояснюючи *чому* хтось міг би зацікавитися читанням текстового файлу.

Як: Приклади коду та вихідні дані у блоках коду "```Gleam ... ```".

Щоб глибоко зануритися: Більш детальна інформація про читання текстового файлу.

## Чому
Читання текстового файлу може бути необхідною задачею для багатьох програм у мові програмування Gleam. Це може бути використано для отримання даних з бази даних, обробки великих файлів або навіть для зчитування конфігураційних файлів.

## Як
Простий спосіб прочитати текстовий файл у Gleam - використати функцію `read_lines` з модуля `gleam/io/file`. Наприклад, якщо у нас є файл з назвою `text.txt` з таким вмістом:

```
Hello, world!
This is a sample text file.
```

Цей код може прочитати всі рядки файлу та вивести їх на екран:

```Gleam
import gleam/io/file
file_path = "text.txt"
lines = file.read_lines(file_path)
file.iter(lines, fn(line) {
  io.print("Line: {}", [line])
})
```

Виведе на екран:

```
Line: Hello, world!
Line: This is a sample text file.
```

## Глибока занурення
У Gleam також є потужні засоби для роботи з текстовими файлами. Наприклад, ви можете використовувати модуль `gleam/io/text` для обробки тексту за допомогою регулярних виразів, конвертування у кодування Unicode та інших операцій.

## Додатково
Для отримання додаткової інформації про роботу з текстовими файлами в Gleam, перегляньте наступні документаційні ресурси:

- Документація модуля `gleam/io/file`: https://gleam.run/modules/gleam_io_file.html
- Документація модуля `gleam/io/text`: https://gleam.run/modules/gleam_io_text.html

## Дивіться також
- Робота з об'єктами JSON у мові Gleam: https://yourlinkhere.com
- Робота з базою даних SQLite у мові Gleam: https://yourlinkhere.com