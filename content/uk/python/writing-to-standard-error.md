---
title:                "Запис в стандартний потік помилок"
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це таке & Чому?

Стандартна помилка (stderr) - спеціальний потік для виводу помилок та діагностичних повідомлень. Програмісти виводять на нього помилки, щоб відокремлювати їх від основного виводу програми (stdout).

## Як це робити:

```Python
import sys

print("Це йде до стандартного виводу.")
sys.stderr.write("Це йде до стандартної помилки.\n")
```

Вивід:

```
Це йде до стандартного виводу.
Це йде до стандартної помилки.
```

## Поглиблено:

Стандартна помилка `stderr` використовується з часів Unix для виведення помилок у консоль, щоб їх можна було відокремити чи перенаправити. `sys` у Python дозволяє доступитися до `stderr`. Як альтернатива, можна використовувати `logging` модуль для більш гнучкого контролю над виводом помилок. У більшості Unix-подібних систем `stderr` має файловий дескриптор `2`.

## Дивись також:

- [Документація Python модуля sys](https://docs.python.org/3/library/sys.html)
- [Стаття про стандартні потоки на Wikipedia](https://uk.wikipedia.org/wiki/Стандартні_потоки)
- [Документація Python модуля logging](https://docs.python.org/3/library/logging.html)
