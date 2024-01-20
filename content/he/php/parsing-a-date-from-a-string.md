---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

פריסת תאריך ממחרוזת היא פעולה שבה הופכים מחרוזת, כמו "25/12/2020", לתאריך תקני למחשב. מתכנתים מבצעים פעולה זו כדי להפוך נתונים שהמשתמש מזין לפורמט שיכול להיעבד על ידי המחשב.

## איך עושים:

באמצעות הפונקציה `date_parse` של PHP, אנחנו יכולים לעבד מחרוזת ולהמירה לתאריך.
```PHP
$string_date = "2020-12-25";
$parsed_date = date_parse($string_date);
var_dump($parsed_date);
```
זה יפיק:
```PHP
array(11) {
  ["year"]=>
  int(2020)
  ["month"]=>
  int(12)
  ["day"]=>
  int(25)
  ...
}
```

## צלילה עמוקה:

פונקציה `date_parse` הופיעה לראשונה ב-php5.2.0. ישנן גם חלופות אחרות כמו `DateTime::createFromFormat`.

פירוט המידע מהפונקציה נמצא במערך, כאשר מכיל חדשים מזוהים שונים, כולל שנה, חודש, יום, שעה, דקות, שניות ולא רק.

## ראו גם:

- תיעוד של פונקציה `date_parse` : https://www.php.net/manual/en/function.date-parse.php
- איך להשתמש ב-`DateTime::createFromFormat` : https://www.php.net/manual/en/datetime.createfromformat.php
- טיפים נוספים על תאריכים ב-PHP: https://www.w3schools.com/php/php_date.asp