---
title:                "המרת מחרוזת לאותיות קטנות"
aliases:
- /he/php/converting-a-string-to-lower-case/
date:                  2024-01-20T17:39:46.260117-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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
