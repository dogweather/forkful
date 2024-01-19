---
title:                "Перевірка наявності директорії"
html_title:           "Clojure: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо це потрібно?

Перевірка наявності директорії - це процес виявлення, чи існує вказаний шлях до папки в файловій системі типу Unix або Windows. Програмісти роблять це, щоб уникнути помилок при спробі відкриття або запису файлів у неіснуючу директорію.

## Як це зробити:

```PHP
// Процес перевірки, чи існує директорія

$dir = '/path/to/your/directory';

if(is_dir($dir)){
    echo "Директорія існує";
} else {
    echo "Директорія не існує";
}
```

## Поглиблений аналіз:

1. Про історію: У ранніх версіях PHP функція `is_dir` була найефективнішим способом перевірки наявності директорії.
2. Про альтернативи: Але ви також можете використати функцію `file_exists()`, яка перевіряє, чи існує файл або директорія.
3. Про деталі реалізації: `is_dir()` є вбудованою функцією PHP, яка використовує системні виклики операційної системи для перевірки наявності директорії.

## Дивіться також:

1. [PHP: file_exists - Manual](https://www.php.net/manual/en/function.file-exists.php)
2. [PHP: is_dir - Manual](https://www.php.net/manual/en/function.is-dir.php)
3. [PHP: opendir - Manual](https://www.php.net/manual/en/function.opendir.php)