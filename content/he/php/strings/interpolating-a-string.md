---
date: 2024-01-20 17:51:30.073422-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E2\u05D5\u05D3\
  \ \u05D3\u05D5\u05D2\u05DE\u05D4, \u05E2\u05DD \u05EA\u05D7\u05D1\u05D9\u05E8 \u05DE\
  \u05D5\u05E8\u05DB\u05D1 \u05D9\u05D5\u05EA\u05E8."
lastmod: '2024-04-05T21:53:40.617868-06:00'
model: gpt-4-1106-preview
summary: "\u05E2\u05D5\u05D3 \u05D3\u05D5\u05D2\u05DE\u05D4, \u05E2\u05DD \u05EA\u05D7\
  \u05D1\u05D9\u05E8 \u05DE\u05D5\u05E8\u05DB\u05D1 \u05D9\u05D5\u05EA\u05E8."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## איך לעשות:
```PHP
$name = 'דוד';
$greeting = "שלום, $name!";
echo $greeting; // ידפיס: שלום, דוד!
```

עוד דוגמה, עם תחביר מורכב יותר:
```PHP
$item = 'כובע';
$price = 20;
echo "המחיר של ה$item הוא: {$price} ש"ח"; // ידפיס: המחיר של הכובע הוא: 20 ש"ח
```

## עיון נוסף
בעבר, היינו צריכים לשלב מחרוזות ומשתנים בעזרת הפעולה `.` לצרפם יחד, אבל החל מ-PHP 5.0, אינטרפולציה הפכה להיות עניין פשוט יותר. יש גם אפשרות לעשות את זה בעזרת פונקציות למחרוזות כמו `sprintf` או `printf`. כשאנחנו משתמשים בתחביר מורכב או בתוך מחרוזת עם מרכאות כפולות, פי.אח.פי. מחפשת את המשתנה ומחליפה אותו בערך שלו.

## ראה גם
- [PHP: מחרוזות](https://www.php.net/manual/en/language.types.string.php)
- [PHP: משתנים משולבים במחרוזות](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php)
