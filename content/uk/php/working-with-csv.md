---
title:                "Робота з csv"
html_title:           "PHP: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому
Робота з CSV (Comma-Separated Values) є дуже частою задачею для веб-розробників і даних аналітиків. Використовуючи PHP, ви можете легко і швидко опрацьовувати дані з CSV файлів, що дозволить забезпечити ретельний аналіз та обробку значного обсягу даних.

## Як
Щоб почати роботу з CSV файлами у PHP, вам потрібно буде встановити бібліотеку [fgetcsv ()](https://www.php.net/manual/ru/function.fgetcsv.php). Ця функція дозволяє зчитувати дані з CSV файлу і створювати масив з значеннями з кожного рядка. Нижче наведено приклад коду у форматі Markdown:

```
```PHP
<?php
// відкриття файлу
$file = fopen('file.csv', 'r');

// зчитування даних рядка за рядком
while (!feof($file)) {
  // занесення значень у масив
  $data[] = fgetcsv($file);
}

// закриття файлу
fclose($file);

// вивід отриманих значень
print_r($data);
```
Запустивши цей код, ви отримаєте вихідні дані у вигляді масиву. Також, ви можете використовувати цю функцію для запису даних у CSV файл, передаючи потрібні значення у якості аргументів. 

## Deep Dive
Існує багато допоміжних функцій у PHP, які дозволяють працювати з CSV файлами більш ефективно. Наприклад, [fgetcsv ()](https://www.php.net/manual/ru/function.fgetcsv.php) дозволяє задавати відокремлювач полів у вхідному файлі, що є корисним, якщо ви працюєте з CSV файлами, що мають різний формат. Також, можливо використовувати функцію [fputcsv ()](https://www.php.net/manual/ru/function.fputcsv.php) для більш зручного запису даних у CSV файл.

## See Also
- [Розділ до роботи з CSV у PHP](https://www.php.net/manual/ru/refs.fileformats.php)
- [Документація по функції fgetcsv ()](https://www.php.net/manual/ru/function.fgetcsv.php)
- [Приклади готового коду для роботи з CSV у PHP](https://www.php.net/manual/ru/function.fgetcsv.php#refsect1-function.fgetcsv-examples)