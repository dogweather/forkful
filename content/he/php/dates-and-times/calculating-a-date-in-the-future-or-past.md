---
aliases:
- /he/php/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:31:59.536421-07:00
description: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\
  \u05E2\u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8 \u05D4\u05EA\u05D0\u05DE\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05DC\u05E4\u05E0\u05D9 \u05D0\u05D5 \u05D0\u05D7\u05E8\u05D9 \u05E0\
  \u05E7\u05D5\u05D3\u05EA \u05D6\u05DE\u05DF \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DC\u05E6\u05D5\u05E8\u05DA \u05EA\u05DB\u05E0\u05D5\u05DF\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA, \u05DB\
  \u05D2\u05D5\u05DF \u05EA\u05D6\u05DB\u05D5\u05E8\u05D5\u05EA, \u05E1\u05D8\u05D8\
  \u05D9\u05E1\u05D8\u05D9\u05E7\u05D5\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.945872
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D6\u05D4 \u05E4\u05E9\
  \u05D5\u05D8 \u05D4\u05EA\u05D0\u05DE\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05DC\u05E4\u05E0\u05D9 \u05D0\u05D5 \u05D0\u05D7\u05E8\u05D9 \u05E0\u05E7\
  \u05D5\u05D3\u05EA \u05D6\u05DE\u05DF \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DC\u05E6\u05D5\u05E8\u05DA \u05EA\u05DB\u05E0\u05D5\u05DF \u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA, \u05DB\u05D2\
  \u05D5\u05DF \u05EA\u05D6\u05DB\u05D5\u05E8\u05D5\u05EA, \u05E1\u05D8\u05D8\u05D9\
  \u05E1\u05D8\u05D9\u05E7\u05D5\u05EA\u2026"
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר זה פשוט התאמת תאריכים לפני או אחרי נקודת זמן מסוימת. מתכנתים עושים את זה לצורך תכנון פונקציונליות, כגון תזכורות, סטטיסטיקות ותחזיות.

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
