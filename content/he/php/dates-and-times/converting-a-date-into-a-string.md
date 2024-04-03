---
date: 2024-01-20 17:37:31.465547-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4: \u05DB\u05D3\u05D9 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA, \u05D0\u05E4\u05E9\u05E8 \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D9\u05EA\
  \ `date` \u05D0\u05D5 \u05D1\u05DE\u05EA\u05D5\u05D3\u05EA `format` \u05E9\u05DC\
  \ \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 DateTime. \u05E7\u05D1\u05DC\u05D5\
  \ \u05D3\u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.499955-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA, \u05D0\u05E4\u05E9\u05E8 \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D9\u05EA\
  \ `date` \u05D0\u05D5 \u05D1\u05DE\u05EA\u05D5\u05D3\u05EA `format` \u05E9\u05DC\
  \ \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 DateTime."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## איך עושים את זה:
כדי להמיר תאריך למחרוזת, אפשר להשתמש בפונקציית `date` או במתודת `format` של אובייקט DateTime. קבלו דוגמה:

```php
<?php
// שימוש בפונקציית date
echo date('Y-m-d'); // יוצא תאריך נוכחי בפורמט YYYY-MM-DD

// שימוש באובייקט DateTime והמתודה format
$date = new DateTime('now');
echo $date->format('Y-m-d H:i:s'); // יהפוך את התאריך למחרוזת עם זמן
?>
```

תוצאה:
```
2023-03-15
2023-03-15 14:35:52
```

## עומק של ידע
ב-PHP, המרת תאריך למחרוזת החלה עם הפונקציה `strftime`, שהפכה לא פעילה ב-PHP 8.0. כיום, אנחנו עובדים עם הפונקציה `date` ואובייקט DateTime. אם יש צורך להמיר תאריכים מפורמט אחד לפורמט אחר, אפשר לעשות זאת בחופשיות עם מתודת `format`. כמו כן, ישנה המחלקה `DateInterval` לפעולות מתקדמות יותר עם תאריכים.

אלטרנטיבות כוללות ספריות חיצוניות כמו Carbon ב-PHP, שמספקת עוד שלל פונקציות לניהול תאריכים. לעומק, כל פורמט שאנחנו מציינים בפונקציות אלו מבוסס על תקני תאריכים של PHP, שניתן למצוא בתיעוד הרשמי.

## ראה גם:
- [תיעוד PHP לפונקציית date](https://www.php.net/manual/en/function.date.php)
- [תיעוד PHP למחלקת DateTime](https://www.php.net/manual/en/class.datetime.php)
- [תקני פורמט של תאריכים מרשמי תיעוד PHP](https://www.php.net/manual/en/datetime.format.php)
- [אתר Carbon](https://carbon.nesbot.com/docs/)
