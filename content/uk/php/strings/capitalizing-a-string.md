---
title:                "Зробити першу літеру рядка великою"
aliases:
- /uk/php/capitalizing-a-string/
date:                  2024-02-03T19:06:17.443064-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Перетворення рядка на великі літери полягає у зміні першого символу даного тексту на велику літеру, забезпечуючи правильний початок речень, заголовків або власних імен у наборі даних. Програмісти часто виконують перетворення рядків на великі літери для нормалізації даних, поліпшення читабельності або забезпечення узгодженості у вводі користувача або обробці текстових даних.

## Як це зробити:
PHP нативно підтримує різні функції для перетворення рядків на великі літери, кожна з яких служить різній меті. Ось як ви можете їх використовувати:

### Перетворення першої літери рядка у велику:

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Виведе: Hello, world!
```

### Перетворення першої літери кожного слова:

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Виведе: Hello, World!
```

### Перетворення всього рядка на великі літери:

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Виведе: HELLO, WORLD!
```

У сценаріях, що вимагають більшого налаштування або використання сторонніх рішень, можна використовувати бібліотеки, такі як `mbstring` (для багатобайтних рядків), особливо коли йдеться про інтернаціоналізацію, де символи можуть виходити за межі основного набору ASCII.

### Використання mbstring для перетворення UTF-8 рядків:

Переконайтеся, що у вашій конфігурації PHP ввімкнено розширення `mbstring`, потім:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Виведе: Élégant
```

Цей підхід допомагає точно перетворювати рядки, що включають не-ASCII символи, дотримуючись нюансів різних мов.
