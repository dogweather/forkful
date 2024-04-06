---
date: 2024-01-20 17:54:54.288791-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) \u0414\u0430\u0432\u043D\u0456\u0448\u0435 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0437\u043C\u0443\u0448\u0435\u043D\u0456\
  \ \u0431\u0443\u043B\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u0432\u0430\u0442\u0438 \u043D\u0438\u0437\u044C\u043A\u043E\u0440\u0456\
  \u0432\u043D\u0435\u0432\u0456 \u043C\u043E\u0432\u0438 \u0442\u0430 \u0444\u0443\
  \u043D\u043A\u0446\u0456\u0457, \u0449\u043E\u0431 \u0447\u0438\u0442\u0430\u0442\
  \u0438 \u0444\u0430\u0439\u043B\u0438. \u041F\u0430\u0439\u0442\u043E\u043D \u043C\
  \u0430\u0454 \u0432\u0438\u0441\u043E\u043A\u043E\u0440\u0456\u0432\u043D\u0435\u0432\
  \u0435 API\u2026"
lastmod: '2024-04-05T22:51:01.782809-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ \u0414\u0430\u0432\u043D\u0456\u0448\u0435 \u043F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u0437\u043C\u0443\u0448\u0435\u043D\u0456 \u0431\
  \u0443\u043B\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0432\u0430\u0442\u0438 \u043D\u0438\u0437\u044C\u043A\u043E\u0440\u0456\u0432\u043D\
  \u0435\u0432\u0456 \u043C\u043E\u0432\u0438 \u0442\u0430 \u0444\u0443\u043D\u043A\
  \u0446\u0456\u0457, \u0449\u043E\u0431 \u0447\u0438\u0442\u0430\u0442\u0438 \u0444\
  \u0430\u0439\u043B\u0438."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
