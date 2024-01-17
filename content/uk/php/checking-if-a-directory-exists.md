---
title:                "Перевірка наявності директорії"
html_title:           "PHP: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

README.md

## Що і чому?

Перевірка наявності каталогу є важливою складовою для багатьох програмістів. Це дозволяє вам переконатися, що потрібна директорія існує перед виконанням подальших дій з файлами в ній.

## Як це зробити:

Найпростіший спосіб перевірити, чи існує каталог в PHP - використовувати функцію `is_dir()`. Ось приклад коду та його вивід:

```PHP
if (is_dir("my_directory")) {
    echo "Каталог знайдено.";
} else {
    echo "Каталог не існує.";
}
```

Вивід:

```PHP
Каталог знайдено.
```

## Що варто знати:

Перевірка наявності каталогу була реалізована у PHP 4. Ви також можете використовувати функцію `file_exists()` для перевірки наявності будь-якого типу файлу, а не тільки каталогів. Також є альтернативний метод - використовувати команду `shell_exec()` для запуску команди `cd` у системному шелі та перевірки поверненого результату.

## Дивіться також:

- [Функція is_dir() у документації PHP](https://www.php.net/manual/en/function.is-dir.php)
- [Функція file_exists() у документації PHP](https://www.php.net/manual/en/function.file-exists.php)
- [Функція shell_exec() у документації PHP](https://www.php.net/manual/en/function.shell-exec.php)