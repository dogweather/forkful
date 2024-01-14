---
title:    "Python: Створення тимчасового файлу"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файла може бути корисною технікою в програмуванні, особливо коли потрібно зберігати тимчасові дані або створювати тимчасові файли для обробки даних.

## Як створити тимчасовий файл в Python

Для створення тимчасового файлу використовується модуль `tempfile` в Python. Можна створити тимчасовий файл за допомогою методу `NamedTemporaryFile()`:

```Python
import tempfile

temp_file = tempfile.NamedTemporaryFile()
```

Лише з цим кодом, тимчасовий файл буде створений з випадково обраною назвою у тимчасовій директорії системи. Якщо потрібно вказати певну назву для тимчасового файлу, можна використати параметр `prefix`:

```Python
import tempfile

temp_file = tempfile.NamedTemporaryFile(prefix="temp_", suffix=".txt")
```

Також можна вказати шлях для тимчасового файлу, використовуючи параметр `dir`:

```Python
import tempfile

temp_file = tempfile.NamedTemporaryFile(dir="path/to/directory")
```

Отримати назву тимчасового файлу можна за допомогою атрибуту `name`:

```Python
print(f"Назва тимчасового файлу: {temp_file.name}")
```

## Детальний аналіз

Створення тимчасових файлів може зберегти час та зусилля у програмуванні. Крім того, за допомогою модуля `tempfile` також можна використовувати тимчасові каталоги і симлінки.

У разі потреби тимчасовий файл можна видалити вручну за допомогою методу `.close()`:

```Python
temp_file.close()
```

## Дивіться також

- [Документація Python про модуль tempfile](https://docs.python.org/3/library/tempfile.html)
- [Корисні техніки для роботи з тимчасовими файлами в Python](https://www.toptal.com/python/python-temporary-files)