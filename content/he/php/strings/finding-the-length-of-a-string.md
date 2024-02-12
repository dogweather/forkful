---
title:                "מציאת אורך מחרוזת"
aliases:
- /he/php/finding-the-length-of-a-string/
date:                  2024-01-20T17:48:15.851056-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת ב-PHP זה כמו למדוד את קו הרוחב של הספה שלך; אתה צריך לדעת את המרחב שהיא תופסת. תכניתנים עושים זאת כדי לוודא שנתונים תואמים לתקנים, למנוע את גלישת התוכנה, או לעשות פעולות עיבוד מחרוזות.

## איך עושים את זה:
PHP משתמשת בפונקציה קלאסית בשם `strlen()` כדי לקבל את אורך המחרוזת. קחו את הדוגמה הבאה:

```PHP
<?php
$text = "שלום עולם";
$length = strlen($text);
echo $length;
?>
```

פלט:

```
16
```

הפלט מראה 16 ולא 9, שכן כל תו בעברית מורכב משני בתים (Bytes).

## צלילה לעומק:
לפני ש-PHP הייתה עם תמיכה מלאה ב-UTF-8, השימוש ב-`strlen()` יכול היה להוביל לבלבול כמו בדוגמה למעלה. כיום, ישנן אלטרנטיבות כמו `mb_strlen()`, שהיא חלק מההרחבה `mbstring` ותומכת בקידוד רב-ביתי של תווים.

להלן איך להשתמש ב-`mb_strlen()`:

```PHP
<?php
$text = "שלום עולם";
$length = mb_strlen($text, 'UTF-8');
echo $length;
?>
```

פלט:

```
9
```

הפעם הפלט מוצג כ-9, מה שמתאים יותר למספר התווים הנראי שאנו רואים. בחירה בין `strlen()` ל`mb_strlen()` תלויה בהקשר ובצורך של התכנות.

## ראו גם:
- [PHP Manual on strlen()](https://www.php.net/manual/en/function.strlen.php)
- [PHP Manual on mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [mbstring](https://www.php.net/manual/en/book.mbstring.php)
