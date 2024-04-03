---
date: 2024-01-20 17:48:15.851056-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4: PHP \u05DE\u05E9\u05EA\u05DE\u05E9\u05EA \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D4 \u05E7\u05DC\u05D0\u05E1\u05D9\u05EA \u05D1\u05E9\u05DD `strlen()`\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA \u05D0\u05D5\u05E8\u05DA\
  \ \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05E7\u05D7\u05D5 \u05D0\u05EA \u05D4\
  \u05D3\u05D5\u05D2\u05DE\u05D4 \u05D4\u05D1\u05D0\u05D4."
lastmod: '2024-03-13T22:44:39.466524-06:00'
model: gpt-4-1106-preview
summary: "PHP \u05DE\u05E9\u05EA\u05DE\u05E9\u05EA \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D4 \u05E7\u05DC\u05D0\u05E1\u05D9\u05EA \u05D1\u05E9\u05DD `strlen()`\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA \u05D0\u05D5\u05E8\u05DA\
  \ \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

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
