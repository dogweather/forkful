---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:54:54.288791-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)

Читання текстового файла - це процес отримання даних з файла, збереженого на диску. Програмісти роблять це для обробки, аналізу даних чи налаштування програм через конфігураційні файли.

## How to: (Як це зробити:)

```Python
# Просте читання файлу
with open('example.txt', 'r', encoding='utf-8') as file:
    content = file.read()
    print(content)

# Читання файлу по рядках
with open('example.txt', 'r', encoding='utf-8') as file:
    for line in file:
        print(line.strip())
```

## Deep Dive (Поглиблене Вивчення)

Давніше програмісти змушені були використовувати низькорівневі мови та функції, щоб читати файли. Пайтон має високорівневе API для цього, яке включає автоматичне управління ресурсами (з використанням оператора `with`) та підтримку юнікоду. Існують також альтернативні підходи, наприклад, модулі `csv` або `json` для роботи зі структурованими даними, та бібліотеки як `pandas` для обробки та аналізу даних. Вибір методу залежить від завдань та форматів файлів.

## See Also (Дивіться також)

- [Python Docs: Reading and Writing Files](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python Docs: File and Directory Access](https://docs.python.org/3/library/filesys.html)
- [Real Python: Reading and Writing Files in Python](https://realpython.com/read-write-files-python/)
