---
title:                "הסרת מרכאות ממחרוזת"
aliases:
- /he/php/removing-quotes-from-a-string/
date:                  2024-01-26T03:41:33.611559-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת מרכאות ממחרוזת ב-PHP משמעותה הוא שלילת תווי המרכאות הכפולות (`"`) או היחידות (`'`) המציקים אשר יכולים לבלבל את הלוגיקה של הקוד או את שאילתות מסד הנתונים. מתכנתים עושים זאת כדי לנקות או לסנן נתונים קלט, באופן שמבטיח שמחרוזות נמצאות בשימוש או מאוחסנות באופן בטוח.

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
