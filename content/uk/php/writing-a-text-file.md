---
title:                "Написання текстового файлу"
aliases:
- uk/php/writing-a-text-file.md
date:                  2024-02-03T19:29:00.155085-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написання текстового файлу"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Створення текстового файлу за допомогою PHP передбачає створення або відкриття файлу та вставлення в нього контенту. Програмісти роблять це для збереження даних, як-от контент, створений користувачем, або логи, за межами життєвого циклу програми.

## Як це зробити:
PHP нативно підтримує запис у файл за допомогою функцій, таких як `file_put_contents`, `fopen` разом із `fwrite`, та `fclose`. Ось як їх використовувати:

### Простий запис за допомогою `file_put_contents`:
Ця функція спрощує процес запису у файл, виконуючи все в один крок.
```php
$content = "Привіт, світ!";
file_put_contents("hello.txt", $content);
// Перевіряє, чи файл успішно записаний
if (file_exists("hello.txt")) {
    echo "Файл успішно створено!";
} else {
    echo "Не вдалося створити файл.";
}
```

### Розширений запис за допомогою `fopen`, `fwrite`, та `fclose`:
Для більшого контролю над записом файлу, як-от додавання тексту або краще оброблення помилок, використовуйте `fopen` з `fwrite`.
```php
$file = fopen("hello.txt", "a"); // режим 'a' для додавання, 'w' для запису
if ($file) {
    fwrite($file, "\nДодаємо більше контенту.");
    fclose($file);
    echo "Контент успішно додано!";
} else {
    echo "Не вдалося відкрити файл.";
}
```

#### Читання файлу для виводу:
Щоб перевірити наш контент:
```php
echo file_get_contents("hello.txt");
```
**Приклад виводу:**
```
Привіт, світ!
Додаємо більше контенту.
```

### Використання сторонніх бібліотек:
Для більш складних операцій із файлами можна використовувати бібліотеки, такі як `League\Flysystem`, для створення абстрактного шару над файловою системою, але вбудовані функції PHP часто достатньо для базових завдань запису файлів. Ось короткий приклад, якщо ви вирішите дослідити `Flysystem`:
```php
require 'vendor/autoload.php';
use League\Flysystem\Filesystem;
use League\Flysystem\Local\LocalFilesystemAdapter;

$adapter = new LocalFilesystemAdapter(__DIR__);
$filesystem = new Filesystem($adapter);

$filesystem->write('hello.txt', "Використовуючи Flysystem для запису цього.");
```
Цей приклад передбачає, що ви встановили `league/flysystem` за допомогою Composer. Сторонні бібліотеки можуть значно спростити більш складне оброблення файлів, особливо при роботі з різними системами зберігання безперебійно.
