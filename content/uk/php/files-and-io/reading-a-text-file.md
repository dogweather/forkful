---
title:                "Читання текстового файлу"
aliases:
- /uk/php/reading-a-text-file.md
date:                  2024-01-20T17:54:48.745437-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? | Що та Навіщо?
Читання текстових файлів у PHP - це процес здобуття даних із файлів збережених на диску. Програмісти роблять це, щоб обробити інформацію, зберегти налаштування, або імпортувати дані для веб-додатків.

## How to: | Як це зробити:
```PHP
<?php
// Відкриваємо файл для читання
$file = 'example.txt';
$handle = fopen($file, 'r');

// Перевіряємо, чи файл відкрився
if ($handle) {
    // Читаємо файл по рядку
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }

    // Закриваємо дескриптор файлу
    fclose($handle);
}
?>
```
Sample Output:
```
Це перший рядок вашого текстового файлу.
Це другий рядок файлу.
```

## Deep Dive | Поглиблене Вивчення:
Читання файлів у PHP почалось ще з PHP 3, і функції, такі як `fopen()` і `fgets()`, були основними засобами. Існують альтернативи, як от `file_get_contents()` і `file()` для читання вмісту цілком або масиву рядків відповідно. Під капотом, PHP використовує системні виклики операційної системи, щоб працювати з файлами, забезпечуючи абстракцію для програмістів.

## See Also | Дивіться Також:
- [Офіційна документація PHP по роботі з файлами](https://www.php.net/manual/en/ref.filesystem.php)
- [Інструкція по `fopen()` на php.net](https://www.php.net/manual/en/function.fopen.php)
- [Інструкція по `fgets()` на php.net](https://www.php.net/manual/en/function.fgets.php)
- [Stack Overflow: Best Practices for Reading a File](https://stackoverflow.com/questions/2167079/what-is-the-best-way-in-php-to-read-a-file-for-a-streaming-response)
