---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:38:02.355646-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח תאריך ממחרוזת הוא התהליך שבו אנחנו הופכים טקסט לפורמט תאריך שה-PHP יכול להבין ולעבוד איתו. תכנתים עושים את זה כי מבחינת הקוד, תאריך הוא לא רק מחרוזת - זה מאפשר לנו למנוע שגיאות, להשוות בין תאריכים, ולהפעיל פונקציות תאריך ושעה.

## איך לעשות:
כדי לפענח תאריך, אפשר להשתמש בפונקציית `strtotime` או במחלקת `DateTime`.

```PHP
<?php
$dateString = "2023-04-01 14:00:00";

// עם strtotime:
$timestamp = strtotime($dateString);
echo date("Y-m-d H:i:s", $timestamp); // יוצא "2023-04-01 14:00:00"

// עם DateTime:
$dateTime = new DateTime($dateString);
echo $dateTime->format("Y-m-d H:i:s"); // יוצא "2023-04-01 14:00:00"
?>
```

## צלילה לעומק:
בעבר, `strtotime` הייתה הדרך הנפוצה ביותר לפענח תאריכים ממחרוזות. מאז ש- PHP 5.2 יצא לאוויר, המחלקה `DateTime` הפכה לנפוצה יותר מכיוון שהיא מציעה גמישות רבה יותר וטיפול טוב יותר באזורים זמנים. יתר על כן, כשמשתמשים ב-`DateTime`, אפשר להתעסק עם חריגים, אובייקטים immutable ואפשרויות אחרות שיכולות להיות שימושיות ביישומים מורכבים.

## ראו גם:
- [תיעוד ה-PHP על strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [תיעוד ה-PHP על מחלקת DateTime](https://www.php.net/manual/en/class.datetime.php)
- [מדריך על אזורים זמניים ב-PHP](https://www.php.net/manual/en/timezones.php)
