---
date: 2024-01-20 17:31:59.536421-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.503189-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
```PHP
<?php
// תאריך היום
$today = new DateTime();
echo $today->format('Y-m-d'); // הדפסת התאריך הנוכחי

// חישוב 10 ימים לעתיד
$futureDate = (clone $today)->modify('+10 days');
echo $futureDate->format('Y-m-d'); // הדפסת תאריך 10 ימים מהיום

// חישוב 5 שנים אחורה
$pastDate = (clone $today)->modify('-5 years');
echo $pastDate->format('Y-m-d'); // הדפסת תאריך 5 שנים לפני היום
?>
```
תוצאה:
```
2023-04-01 // תאריך נוכחי
2023-04-11 // 10 ימים לעתיד
2018-04-01 // 5 שנים לעבר
```

## עיון מעמיק:
בעבר, פונקציות כמו `strtotime()` ו-`mktime()` היו הדרך לנהול תאריכים ב-PHP. היום, אנחנו משתמשים ב-`DateTime` class, שמספק גמישות ודיוק רב יותר. אלטרנטיבות ל-`DateTime` כוללות את ההרחבה `DateTimeImmutable`, שמונעת שינוי עצמי באובייקט התאריך, ואת הספרייה `Carbon`, שהיא מעטפת ל-`DateTime` עם תכונות נוספות.

## ראה גם:
- [תיעוד רשמי של PHP לגבי הכיתה DateTime](https://www.php.net/manual/en/class.datetime.php)
- [תיעוד לגבי הפונקציה strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [אתר Carbon, מעטפת ל-DateTime](https://carbon.nesbot.com/)
