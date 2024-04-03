---
date: 2024-01-26 03:41:33.611559-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-PHP \u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\
  \u05D4 \u05D4\u05D5\u05D0 \u05E9\u05DC\u05D9\u05DC\u05EA \u05EA\u05D5\u05D5\u05D9\
  \ \u05D4\u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05D4\u05DB\u05E4\u05D5\u05DC\u05D5\
  \u05EA (`\"`) \u05D0\u05D5 \u05D4\u05D9\u05D7\u05D9\u05D3\u05D5\u05EA (`'`) \u05D4\
  \u05DE\u05E6\u05D9\u05E7\u05D9\u05DD \u05D0\u05E9\u05E8 \u05D9\u05DB\u05D5\u05DC\
  \u05D9\u05DD \u05DC\u05D1\u05DC\u05D1\u05DC \u05D0\u05EA \u05D4\u05DC\u05D5\u05D2\
  \u05D9\u05E7\u05D4 \u05E9\u05DC \u05D4\u05E7\u05D5\u05D3 \u05D0\u05D5 \u05D0\u05EA\
  \ \u05E9\u05D0\u05D9\u05DC\u05EA\u05D5\u05EA \u05DE\u05E1\u05D3\u2026"
lastmod: '2024-03-13T22:44:39.461591-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-PHP \u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\u05D4\
  \ \u05D4\u05D5\u05D0 \u05E9\u05DC\u05D9\u05DC\u05EA \u05EA\u05D5\u05D5\u05D9 \u05D4\
  \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05D4\u05DB\u05E4\u05D5\u05DC\u05D5\u05EA\
  \ (`\"`) \u05D0\u05D5 \u05D4\u05D9\u05D7\u05D9\u05D3\u05D5\u05EA (`'`) \u05D4\u05DE\
  \u05E6\u05D9\u05E7\u05D9\u05DD \u05D0\u05E9\u05E8 \u05D9\u05DB\u05D5\u05DC\u05D9\
  \u05DD \u05DC\u05D1\u05DC\u05D1\u05DC \u05D0\u05EA \u05D4\u05DC\u05D5\u05D2\u05D9\
  \u05E7\u05D4 \u05E9\u05DC \u05D4\u05E7\u05D5\u05D3 \u05D0\u05D5 \u05D0\u05EA \u05E9\
  \u05D0\u05D9\u05DC\u05EA\u05D5\u05EA \u05DE\u05E1\u05D3 \u05D4\u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## איך לעשות זאת:
הנה דוגמה פשוטה באמצעות הפונקציות המובנות של PHP:

```php
$quotedString = "'שלום,' היא אמרה, \"זו יום נפלא!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // פלט: שלום, היא אמרה, זו יום נפלא!
```

פשוט, נכון? הפונקציה `str_replace()` לוקחת מערך של תווים להסרה מהמחרוזת, כולל גם מרכאות כפולות ויחידות.

## ניתוח עמוק
בימים הראשונים של PHP, המפתחים היו צריכים להיות זהירים במיוחד עם מרכאות במחרוזות, במיוחד כאשר מכניסים נתונים למסד נתונים. מרכאות שלא טופלו כראוי יכלו להוביל להתקפות SQL injection. לכן נכנסו ה-'magic quotes', תכונה שהיתה משולבת את נתוני הקלט באופן אוטומטי. תכונה זו הפכה לדחויה ונמחקה לבסוף משום שהיא עודדה תרגולי קידוד גרועים ובעיות אבטחה.

כיום, אנו משתמשים בפונקציות כמו `str_replace()` או regex עם `preg_replace()` עבור דפוסים מתקדמים יותר. הנה דוגמת regex:

```php
$quotedString = "'שלום,' היא אמרה, \"זו יום נפלא!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

עבור נתוני JSON, ייתכן שתשתמשו ב-`json_encode()` עם אפשרויות כמו `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` כדי להימנע מקווים נטויים מיותרים במרכאות שלכם.

כאשר אתם מיישמים, קחו בחשבון מקרי קצה. מה אם המחרוזת שלכם אמורה להכיל מרכאות מסוימות, כמו דיאלוג בסיפור או אינצ'ים במדידות? ההקשר חשוב, אז התאימו את הסרת המרכאות לשימוש המיועד של הנתונים.

## ראו גם
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL Injection Prevention](https://owasp.org/www-community/attacks/SQL_Injection)
