---
title:                "Python: Перевірка наявності папки"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування папки є важливим кроком при роботі з файловою системою в Python. Це дає можливість переконатися, що програма працює з правильною структурою файлів і запобігає виникненню помилок під час роботи з ними.

## Як це зробити

Для перевірки існування папки використовується метод `os.path.exists()`. Цей метод приймає шлях до папки як аргумент і повертає значення `True` або `False`, в залежності від того, чи існує вказана папка за даним шляхом.

```Python
import os

path = "Documents/Project"

if os.path.exists(path):
    print("Папка існує")
else:
    print("Папки не існує")
```

Існує також більш гарячечний спосіб перевірки існування папки за допомогою функції `pathlib.Path.exists()`. Цей метод повертає те ж значення, але використання `pathlib` дозволяє більш зручно працювати з шляхами до файлів і папок.

```Python
from pathlib import Path

path = Path("Documents/Project")

if path.exists():
    print("Папка існує")
else:
    print("Папки не існує")
```

## Глибокий занурення

Варто зазначити, що `os.path.exists()` і функція `pathlib.Path.exists()` не тільки перевіряють існування папок, але також і файлів. Це дозволяє з легкістю маніпулювати з файлами та папками в залежності від їх наявності.

Крім того, при поверненні значення `False`, можливо також виправити помилку і створити нову папку за допомогою методу `os.mkdirs()` або функції `pathlib.Path.mkdir()`.

## Дивись також

- [Документація Python для модуля `os.path`](https://docs.python.org/3/library/os.path.html)
- [Документація Python для модуля `pathlib`](https://docs.python.org/3/library/pathlib.html)
- [Приклади використання методу `os.path.exists()`](https://www.geeksforgeeks.org/how-to-check-if-a-directory-or-file-exists-in-python/)
- [Порівняння `os.path.exists()` та `pathlib.Path.exists()`](https://stackoverflow.com/questions/51965097/what-is-the-simpliest-way-to-check-if-a-directory-exists-in-python)