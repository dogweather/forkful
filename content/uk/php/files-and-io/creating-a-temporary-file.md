---
date: 2024-01-20 17:40:55.937330-07:00
description: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\
  \u0447\u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443\
  \ \u2013 \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441, \u043A\u043E\u043B\u0438\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0430 \u0433\u0435\u043D\u0435\u0440\
  \u0443\u0454 \u0444\u0430\u0439\u043B, \u044F\u043A\u0438\u0439 \u0456\u0441\u043D\
  \u0443\u0432\u0430\u0442\u0438\u043C\u0435 \u043B\u0438\u0448\u0435 \u043F\u0456\
  \u0434 \u0447\u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.463938-06:00'
model: gpt-4-1106-preview
summary: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\
  \u0447\u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443\
  \ \u2013 \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441, \u043A\u043E\u043B\u0438\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0430 \u0433\u0435\u043D\u0435\u0440\
  \u0443\u0454 \u0444\u0430\u0439\u043B, \u044F\u043A\u0438\u0439 \u0456\u0441\u043D\
  \u0443\u0432\u0430\u0442\u0438\u043C\u0435 \u043B\u0438\u0448\u0435 \u043F\u0456\
  \u0434 \u0447\u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\u2026"
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Створення тимчасового файлу – це процес, коли програма генерує файл, який існуватиме лише під час виконання програми. Програмісти використовують це для безпечного зберігання даних, що не потребують постійного збереження, або отримання унікального простору для обробки даних, який легко можна очистити.

## Як це зробити:
PHP надає вбудовані функції для роботи з тимчасовими файлами. Ось як ви можете створити та працювати з тимчасовим файлом:

```php
<?php
$tempFile = tmpfile();

// Перевірка, чи файл успішно створено
if ($tempFile) {
    fwrite($tempFile, "Тут якась тимчасова інформація...\n");

    // Перемотуємо файл, щоб прочитати вміст із початку
    rewind($tempFile);

    // Читаємо вміст файла
    echo fread($tempFile, 1024);

    // Закриваємо файл, він автоматично видалиться
    fclose($tempFile);
} else {
    echo 'Не вдалося створити тимчасовий файл.';
}
?>
```

Приклад виводу:

```
Тут якась тимчасова інформація...
```

## Поглиблений огляд:
Тимчасові файли використовуються в программуванні з давніх часів для забезпечення ізоляції виконання операцій та для зменшення використання оперативної пам'яті. Як альтернативу `tmpfile()`, можна використовувати `tempnam()`, яка створює тимчасовий файл з унікальним ім'ям, або `sys_get_temp_dir()` для отримання шляху до тимчасової папки. Важливо знати, що файли, створені `tmpfile()`, автоматично видаляються після закриття файлу або завершення скрипта, що додає зручності в управлінні ресурсами та безпеці даних.

## Дивіться також:
- [Документація функції tmpfile() в PHP](https://www.php.net/manual/en/function.tmpfile.php)
- [Документація функції tempnam() в PHP](https://www.php.net/manual/en/function.tempnam.php)
- [Документація директиви sys_get_temp_dir() в PHP](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
