---
title:    "Python: Створення тимчасового файлу."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Чому

Створення тимчасових файлів може бути корисним для тих, хто пише програми на Python. Тимчасові файли зазвичай використовуються для тимчасового зберігання даних, які не потрібні на постійній основі.

## Як

Щоб створити тимчасовий файл у Python, ми можемо використовувати модуль ```tempfile```. Спочатку потрібно імпортувати цей модуль у наш код:

```Python
import tempfile
```

Для створення тимчасового файлу нам потрібно використовувати функцію ```tempfile.NamedTemporaryFile()```. Ця функція приймає деякі параметри, але ми можемо не передавати їх, якщо не потрібно. Наприклад, ми можемо використовувати цю функцію для створення тимчасового текстового файлу та запису даних у нього:

```Python
with tempfile.NamedTemporaryFile(mode='w', delete=False) as temp_file:
  temp_file.write("Це тимчасовий файл.")
```

Тут ми використовуємо `mode='w'`, щоб вказати, що ми хочемо відкрити файл для запису. Аргумент `delete=False` вказує на те, що файл не повинен бути видалений автоматично після закриття.

Після того як ми закінчили роботу з файлом, його потрібно закрити за допомогою методу `close()`. Якщо ми використовуємо аргумент `delete=True`, то файл буде видалений після закриття. Існує також можливість використовувати функцію `tempfile.TemporaryFile()`, яка самостійно видаляє тимчасовий файл після закриття.

## Deep Dive

Розглянемо детальніше, як працює створення тимчасового файлу у Python. Коли ми використовуємо функцію `NamedTemporaryFile()`, Python створює тимчасовий файл у системній папці для тимчасових файлів. Звичайно, це буде `/tmp` в UNIX-подібних операційних системах та `C:\Users\<username>\AppData\Local\Temp` в Windows. Але ми можемо вказати власну папку для збереження тимчасових файлів за допомогою аргументу `dir=`.

Також, якщо ми використовуємо аргумент `suffix=`, то ми можемо вказати розширення для створюваного тимчасового файлу. Наприклад, якщо ми встановимо `suffix='.txt'`, то створений файл буде мати розширення `.txt`. 

## See Also

- [Офіційна документація Python](https://docs.python.org/3/library/tempfile.html)
- [Стаття про тимчасові файли в Python (англійською)](https://realpython.com/python-tempfile/)