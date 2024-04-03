---
date: 2024-01-20 17:41:13.360813-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:48.615090-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

## How to:
```python
import tempfile

# Створення тимчасового файлу
with tempfile.TemporaryFile(mode='w+t') as tmp_file:
    # Пишемо у файл
    tmp_file.write('Привіт, Україно!')
    # Повертаємось до початку файлу
    tmp_file.seek(0)
    # Читаємо з файлу
    content = tmp_file.read()
    print(content) # Вивід: Привіт, Україно!

# Файл видалений після виходу з контексту
```
Це базовий приклад. Є ще `tempfile.NamedTemporaryFile` для створення файлу з ім'ям, його ще легше знайти потім.

## Deep Dive
Трохи історії: у UNIX тимчасові файли зазвичай тримають в `/tmp`. Python модуль `tempfile` виник для спрощення роботи з ними. Альтернативи? Можна створювати файли вручну, але чому, якщо є `tempfile`? Як це працює: `tempfile` генерує унікальне ім’я, щоб запобігти конфліктам, і видаляє файли як тільки вони більше не потрібні (якщо використовувати `TemporaryFile` як контекстний менеджер).

## See Also
- [tempfile — Робота з тимчасовими файлами](https://docs.python.org/3/library/tempfile.html)
- [io — Основы вводу/виводу в Python](https://docs.python.org/3/library/io.html)
- [os and shutil — модулі для роботи з файловою системою](https://docs.python.org/3/library/os.html)
