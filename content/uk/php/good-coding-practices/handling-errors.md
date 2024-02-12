---
title:                "Обробка помилок"
date:                  2024-01-26T00:56:59.640576-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обробка помилок"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/handling-errors.md"
---

{{< edit_this_page >}}

## Що і чому?
Обробка помилок у PHP полягає у керуванні та реагуванні на умови, які порушують нормальний хід програми, наприклад, відсутні файли чи некоректні дані на вхід. Програмісти обробляють помилки, щоб запобігти збоям та забезпечити користувачам плавнішу роботу.

## Як це робити:
У PHP ви можете управляти помилками, використовуючи блоки `try-catch`, а також налаштувати процес за допомогою власних обробників помилок та винятків.

```php
// Основний приклад try-catch
try {
  // Спроба виконати ризиковану дію
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Обробка помилки
  echo "Помилка: " . $e->getMessage();
}

// Налаштування власного обробника помилок
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Використання винятків
class MyException extends Exception {}

try {
  // Здійснити якусь дію та винести власний виняток
  throw new MyException("Користувацька помилка!");
} catch (MyException $e) {
  // Обробка користувацького винятку
  echo $e->getMessage();
}

// Приклад виводу:
// Помилка: fopen(nonexistentfile.txt): не вдалось відкрити потік: Файл чи директорія не існує
// Користувацька помилка!
```

## Поглиблене вивчення
У минулому помилки PHP були швидше попередженнями та повідомленнями, які не зупиняли виконання скрипта. З часом, коли мова еволюціонувала, вона прийняла більш надійну об'єктно-орієнтовану обробку помилок через клас Exception, введений у PHP 5. Пізніше, у PHP 7 з'явилися класи Error, які нарешті диференціюють помилки та винятки.

До блоків `try-catch`, у PHP використовували `set_error_handler()` для роботи з помилками. `try-catch` є чистішим, сучаснішим. Але власні обробники помилок все ще мають місце, особливо для історичного коду або коли вам потрібно вловити те, що зазвичай було б невинятковими помилками.

Інтерфейс `Throwable` у PHP 7+ означає, чи це Error, чи Exception, ви можете ловити обидва. Це зручно, оскільки тепер ви не пропускаєте критичні помилки виконання, які були складнішими для відстеження раніше.

Альтернативи вбудованим механізмам PHP включають бібліотеки та фреймворки, які приходять з власними системами обробки помилок, пропонуючи більше можливостей, як-от логування помилок у файли чи відображення сторінок помилок, зручних для користувача.

## Дивіться також
- Офіційна документація PHP про винятки: https://www.php.net/manual/uk/language.exceptions.php
- PHP The Right Way про звіт про помилки: https://phptherightway.com/#error_reporting
- Руковідство PHP з обробки помилок: https://www.php.net/manual/uk/book.errorfunc.php