---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- /he/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:59.536421-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/calculating-a-date-in-the-future-or-past.md"
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
