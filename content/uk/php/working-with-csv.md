---
title:                "PHP: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Можливо, ви хочете працювати з CSV для збереження та обробки великих обсягів даних за допомогою PHP. CSV є одним з найпоширеніших форматів для збереження даних, особливо для експорту та імпорту з баз даних, електронних таблиць та інших програм.

## Як

Найпростішим способом роботи з CSV у PHP є використання функцій `fopen`, `fputcsv` та `fgetcsv`. Давайте подивимося на швидкий приклад роботи з CSV файлом за допомогою цих функцій:

```PHP
// Відкриття CSV файлу для запису у режимі додавання
$csv_file = fopen('my_data.csv', 'a');

// Запис рядка даних у CSV файл
$data = ['John', 'Doe', 'jsdoe@example.com'];
fputcsv($csv_file, $data);

// Закриття файлу
fclose($csv_file);

// Відкриття CSV файлу для читання
$csv_file = fopen('my_data.csv', 'r');

// Читання рядка даних з CSV файлу та виведення його
while ($row = fgetcsv($csv_file)) {
    echo 'Name: ' . $row[0] . '<br>';
    echo 'Last Name: ' . $row[1] . '<br>';
    echo 'Email: ' . $row[2] . '<br>';
}

// Закриття файлу
fclose($csv_file);
```

Вивід прикладу:

```bash
Name: John
Last Name: Doe
Email: jsdoe@example.com
```

## Deep Dive

Навіть якщо ви вже знаєте, як працювати з CSV у PHP, є деякі важливі аспекти, на які варто звернути увагу. Ось деякі корисні поради для роботи з CSV:

- Використання циклів `foreach` для роботи з даними рядка в CSV файлі.
- Перевірка наявності та правильного формату CSV файлу перед обробкою даних.
- Врахування можливості наявності спеціальних символів у даних, таких як коми або подвійні лапки, які можуть спричинити проблеми при читанні або записі у файл.
- Використання функцій `fgetcsv` та `fputcsv` з параметром роздільників, якщо вихідні дані відрізняються від звичайного коми.
- Використання функцій `trim` та `htmlspecialchars` для очищення та забезпечення безпеки даних перед записом у файл.

## Дивіться також

- [Офіційна документація PHP для роботи з CSV](https://www.php.net/manual/en/ref.csv.php)
- [Поради щодо ефективного роботи з CSV в PHP](https://www.ownyourbits.com/2017/03/03/efficiently-read-and-write-from-and-to-csv-in-php/)
- [Просте використання бібліотеки `league/csv` для роботи з CSV у PHP](https://csv.thephpleague.com/)