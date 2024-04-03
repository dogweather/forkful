---
date: 2024-01-20 17:39:46.260117-07:00
description: "\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA\
  \ \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05D1\u05D5 \u05DB\u05DC \u05D4\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D4\u05D2\u05D3\u05D5\u05DC\u05D5\u05EA\
  \ \u05D1\u05EA\u05D5\u05DA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E9\
  \u05EA\u05E0\u05D5\u05EA \u05DC\u05E7\u05D8\u05E0\u05D5\u05EA. \u05E4\u05E8\u05D5\
  \u05D2\u05E8\u05DE\u05D9\u05E1\u05D8\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D3 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05DC\u05E4\u05E0\u05D9 \u05D4\u05E9\u05D5\u05D5\u05D0\u05D4\
  \ \u05D0\u05D5 \u05E2\u05D9\u05D1\u05D5\u05D3\u2026"
lastmod: '2024-03-13T22:44:39.460056-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D4\
  \u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05D1\u05D5 \u05DB\u05DC \u05D4\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05D4\u05D2\u05D3\u05D5\u05DC\u05D5\u05EA \u05D1\
  \u05EA\u05D5\u05DA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E9\u05EA\
  \u05E0\u05D5\u05EA \u05DC\u05E7\u05D8\u05E0\u05D5\u05EA."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## מה ולמה?
המרה של מחרוזת לאותיות קטנות היא תהליך בו כל האותיות הגדולות בתוך המחרוזת משתנות לקטנות. פרוגרמיסטים עושים זאת כדי לאחד נתונים לפני השוואה או עיבוד נתונים, ולאפשר עקביות ורגישות נמוכה לרישיות.

## איך לעשות:
ב-PHP, המרת טקסט לאותיות קטנות פשוטה למדי. הפונקציה `strtolower()` תעשה את העבודה בשבילך. נתחיל:

```PHP
<?php
$originalString = "Shalom, OLAM!";
$lowercaseString = strtolower($originalString);
echo $lowercaseString;  // יודפס: "shalom, olam!"
?>
```

וזהו, פשוט כמו פיתת חומוס.

## צלילה עמוקה:
המרה של מחרוזות לאותיות קטנות אינה מושג חדש בתחום התכנות. מאז שנות ה-70 ו-80, תוכניות טקסט מבצעות המרות כאלה לצורך מיון או חיפוש טקסט.

ב-PHP, `strtolower()` היא לא האופציה היחידה. פונקציות מולטיבייט כמו `mb_strtolower()` נותנות פתרון מלא יותר שמתמוך באנקודינג של UTF-8, ועל כן יותר מתאימות לעבודה עם טקסט בבינלאומיות.

אם נמשיך לדבר על פנים טכניות, `strtolower()` עושה שימוש בטבלת האנקודינג של התווים בשפה. לכל אות גדולה יש אות קטנה מתאימה, והפונקציה מבצעת את ההמרה הזו באופן ישיר.

## ראה גם:
- [Strtolower Manual](https://www.php.net/manual/en/function.strtolower.php) - מדריך ה-php.net עבור `strtolower()`
- [Mb_strtolower Manual](https://www.php.net/manual/en/function.mb-strtolower.php) - מדריך ה-php.net עבור `mb_strtolower()`
- [Unicode Case Mapping](https://unicode.org/reports/tr21/tr21-5.html) - מידע על המיפוי של אותיות גדולות וקטנות ביוניקוד
