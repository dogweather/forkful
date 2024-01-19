---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що та чому?

Получення поточної дати це процес відображення дати, що відповідає вашому поточному часовому поясу. Програмісти це роблять, щоб відстежувати та відображати часові мітки подій, іноді вносити зміни, що залежать від часу.

## Як це зробити:

Отримайте поточну дату та час у PHP за допомогою наступного коду:

```PHP
<?php
    echo date('Y-m-d H:i:s');
?>
```

При виконанні цього коду, ви отримаєте відповідь у форматі `YYYY-MM-DD HH:MM:SS`, який відображатиме поточну дату та час.

## Поглиблено:

1. Історичний контекст: Функція `date()` була введена в PHP 4 і продовжує бути суттєвим елементом мови PHP.
2. Альтернативи: Ви також можете використовувати клас `DateTime` для отримання поточної дати та часу: 

    ```PHP
    <?php
        $currentDate = new DateTime();
        echo $currentDate->format('Y-m-d H:i:s');
    ?>
    ```

3. Деталі реалізації: Функція `date()` у PHP використовує серверний часовий пояс. Якщо ви хочете використовувати специфічний часовий пояс, ви можете встановити його за допомогою функції `date_default_timezone_set()`.
   
    ```PHP
    <?php
        date_default_timezone_set('Europe/Kiev');
        echo date('Y-m-d H:i:s');
    ?>
    ```

## Дивіться також:

* [Офіційна документація PHP по функції date()](https://www.php.net/manual/en/function.date.php)
* [Офіційна документація PHP по класу DateTime](https://www.php.net/manual/en/class.datetime.php)
* [Офіційна документація PHP по функції date_default_timezone_set()](https://www.php.net/manual/en/function.date-default-timezone-set.php)

Пам'ятайте, що працювати з датами та часом може бути складно через різницю в часових поясах і форматах дати. Отже, завжди перевіряйте свій код на різних випадках.