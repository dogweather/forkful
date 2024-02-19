---
aliases:
- /he/php/finding-the-length-of-a-string/
date: 2024-01-20 17:48:15.851056-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-PHP \u05D6\u05D4 \u05DB\u05DE\u05D5\
  \ \u05DC\u05DE\u05D3\u05D5\u05D3 \u05D0\u05EA \u05E7\u05D5 \u05D4\u05E8\u05D5\u05D7\
  \u05D1 \u05E9\u05DC \u05D4\u05E1\u05E4\u05D4 \u05E9\u05DC\u05DA; \u05D0\u05EA\u05D4\
  \ \u05E6\u05E8\u05D9\u05DA \u05DC\u05D3\u05E2\u05EA \u05D0\u05EA \u05D4\u05DE\u05E8\
  \u05D7\u05D1 \u05E9\u05D4\u05D9\u05D0 \u05EA\u05D5\u05E4\u05E1\u05EA. \u05EA\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05E7\u05E0\
  \u05D9\u05DD,\u2026"
lastmod: 2024-02-18 23:08:52.922474
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-PHP \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\
  \u05DE\u05D3\u05D5\u05D3 \u05D0\u05EA \u05E7\u05D5 \u05D4\u05E8\u05D5\u05D7\u05D1\
  \ \u05E9\u05DC \u05D4\u05E1\u05E4\u05D4 \u05E9\u05DC\u05DA; \u05D0\u05EA\u05D4 \u05E6\
  \u05E8\u05D9\u05DA \u05DC\u05D3\u05E2\u05EA \u05D0\u05EA \u05D4\u05DE\u05E8\u05D7\
  \u05D1 \u05E9\u05D4\u05D9\u05D0 \u05EA\u05D5\u05E4\u05E1\u05EA. \u05EA\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05E7\u05E0\u05D9\
  \u05DD,\u2026"
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
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
