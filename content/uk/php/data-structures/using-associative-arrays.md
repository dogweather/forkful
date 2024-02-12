---
title:                "Використання асоціативних масивів"
date:                  2024-01-30T19:14:22.215227-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання асоціативних масивів"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Асоціативні масиви в PHP — це як надзвичайно потужні списки, де кожен елемент може бути доступний за допомогою зрозумілого людині ключа, а не просто чисел. Програмісти використовують їх для зберігання та маніпуляції даними більш інтуїтивно, що дозволяє писати код, який легше читати і легше підтримувати.

## Як:

У PHP створення та використання асоціативних масивів є простим. Ось короткий опис:

```PHP
<?php
// Створення асоціативного масиву
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// Альтернативно, короткий синтаксис масивів
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// Доступ до значень за допомогою ключів
echo "Ім'я: " . $person["name"] . "\n";
echo "Вік: " . $person["age"] . "\n";
echo "Ел. пошта: " . $person["email"] . "\n";

// Зміна значення
$person["age"] = 31;

// Додавання нової пари ключ-значення
$person["country"] = "USA";

// Ітерація по асоціативному масиву
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "\n";
}

// Вивід
// Ім'я: John Doe
// Вік: 31
// Ел. пошта: john@example.com
// країна: USA
?>
```

Зверніть увагу, як ключі можуть бути будь-яким рядком, що дозволяє вам отримувати доступ до елементів за допомогою цих ключів, а не числових індексів, які можуть бути менш значущими та важчими для запам'ятовування.

## Глибше занурення

Асоціативні масиви в PHP внутрішньо реалізовані за допомогою хеш-таблиць, які забезпечують дуже швидкий доступ до елементів за ключем, роблячи їх високоефективними для багатьох завдань. Ця ефективність, разом з їх легкістю використання, робить асоціативні масиви каменем основи програмування на PHP.

Історично масиви в PHP (як індексовані, так і асоціативні) були неймовірно гнучкими, дозволяючи їм слугувати списками, стеками, чергами та багато іншим. Однак, ця гнучкість іноді може призвести до менш ефективного коду, якщо не використовувати її обережно.

Нещодавно, з удосконаленнями об'єктно-орієнтованого програмування в PHP, деякі розробники віддають перевагу використанню об'єктів для структурованих даних, особливо для складних або взаємопов'язаних наборів даних. Використання класів може пропонувати кращу інкапсуляцію та абстракцію, робити код легшим для тестування та уточнювати наміри. Однак, для простого зберігання ключ-значення та прямолінійних сценаріїв маніпуляції даними, асоціативні масиви залишаються відмінним вибором завдяки їх простоті та інтуїтивному синтаксису.