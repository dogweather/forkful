---
date: 2024-01-20 17:54:48.745437-07:00
description: "How to: | \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: ."
lastmod: '2024-03-13T22:44:49.460438-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
