---
title:                "Створення тимчасового файлу"
date:                  2024-01-20T17:41:13.360813-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Тимчасові файли: для чого і навіщо? Вони допомагають зберігати дані, що не потрібно зберігати постійно. Використовуємо для тестування, обробки великих даних, або коли не хочемо захаращувати диск.

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
