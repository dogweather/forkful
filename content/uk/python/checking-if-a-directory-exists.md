---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:58:14.224398-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Перевірка існування каталогу дозволяє коду уникнути помилок перед доступом до файлів. Програмісти перевіряють каталоги, щоб гарантувати, що код не зламається, намагаючись читати або записувати відсутній шлях.

## How to (Як це зробити):
```Python
import os

# Перевірити, чи існує каталог
directory = "/path/to/your/directory"
if os.path.isdir(directory):
    print("Каталог існує!")
else:
    print("Каталог не знайдено.")

# Альтернативний спосіб з використанням pathlib
from pathlib import Path

directory = Path("/path/to/your/directory")
if directory.is_dir():
    print("Каталог існує!")
else:
    print("Каталог не знайдено.")
```

## Deep Dive (Поглиблений Огляд):
Перевірка на існування каталогу в Python існує з давніх часів, починаючи з модуля `os`. З версії 3.4 Python пропонує новий модуль `pathlib`, який робить роботу з шляхами файлив більш інтуїтивно зрозумілою. До того ж, `pathlib` є об'єктно-орієнтованим і часто більш зручним. Обидва підходи правильні, але `pathlib` може виглядати більш сучасно і "pythonic".

Щодо альтернатив, у деяких сценаріях може бути корисним створити каталог, якщо він не існує. Це можна зробити за допомогою `os.makedirs(directory, exist_ok=True)` або `Path(directory).mkdir(parents=True, exist_ok=True)`.

## See Also (Додатково):
- Документацію по основним функціям модуля `os.path`: https://docs.python.org/3/library/os.path.html
- Документацію по `pathlib` для комплексної роботи з шляхами: https://docs.python.org/3/library/pathlib.html
- PEP 428 — Пропозиція щодо `pathlib`: https://www.python.org/dev/peps/pep-0428/
