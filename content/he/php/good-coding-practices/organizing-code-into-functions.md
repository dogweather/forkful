---
date: 2024-01-26 01:16:53.593689-07:00
description: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\
  \u05DA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05D3\u05D5\u05D1\
  \u05E8 \u05E2\u05DC \u05E4\u05D9\u05E8\u05D5\u05E7 \u05D4\u05E7\u05D5\u05D3 \u05E9\
  \u05DC\u05DA \u05DC\u05D1\u05DC\u05D5\u05E7\u05D9\u05DD \u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D7\u05D5\u05D6\u05E8 \u05E2\
  \u05DD \u05DE\u05D8\u05E8\u05D5\u05EA \u05DE\u05D5\u05D2\u05D3\u05E8\u05D5\u05EA\
  . \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC \u05E1\u05D3\
  \u05E8, \u05DC\u05DE\u05E0\u05D5\u05E2 \u05D7\u05E4\u05D9\u05E4\u05D5\u05EA, \u05D5\
  \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.489182-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05D3\u05D5\u05D1\u05E8\
  \ \u05E2\u05DC \u05E4\u05D9\u05E8\u05D5\u05E7 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\
  \u05DA \u05DC\u05D1\u05DC\u05D5\u05E7\u05D9\u05DD \u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D7\u05D5\u05D6\u05E8 \u05E2\u05DD\
  \ \u05DE\u05D8\u05E8\u05D5\u05EA \u05DE\u05D5\u05D2\u05D3\u05E8\u05D5\u05EA. \u05D0\
  \u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC \u05E1\u05D3\u05E8\
  , \u05DC\u05DE\u05E0\u05D5\u05E2 \u05D7\u05E4\u05D9\u05E4\u05D5\u05EA, \u05D5\u05DC\
  \u05D4\u05E4\u05D5\u05DA \u05D0\u05EA\u2026"
title: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
ארגון קוד לתוך פונקציות מדובר על פירוק הקוד שלך לבלוקים ניתנים לשימוש חוזר עם מטרות מוגדרות. אנחנו עושים את זה כדי לשמור על סדר, למנוע חפיפות, ולהפוך את האיתור של באגים לקל יותר.

## איך לעשות:
נדמיין שיש לנו קוד חוזר על עצמו לברכת משתמשים. במקום זאת, נחבא אותו בתוך פונקציה כמו `greet_user`:

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

פלט:
```
Hello, Alice!
Hello, Bob!
```

עכשיו, יש לך כלי נוח שאתה יכול להשתמש בו בכל עת ללא צורך לכתוב מחדש את אותן שורות קוד כל פעם שתרצה לומר שלום.

## עיון עמוק יותר
פונקציות היו בתכנות מימי ה-FORTRAN בשנות ה-50. הן מהוות אבן פינה של תכנות מבני ועוסקות במודולריות ובידוד. חלופות? וב, אפשר ללכת לכיוון המונחה-עצמים ולדבר על מחלקות ומתודות, שהן פונקציות עם חליפה מנומרת. לגבי PHP, פרטי היישום כוללים הגדרת ערכים ברירת מחדל לפרמטרים, הצהרה על סוגי קלט, ויכולת להחזיר מספר ערכים באמצעות מערך או, החל מ-PHP 7.1 ואילך, רשימה.

הנה פניה מודרנית עם הצהרת סוג וערכי ברירת מחדל:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 הביא גם פונקציות חץ, שעוזרות לכתוב פונקציות חד-שורה תמציתיות, שנפוצות במיוחד בפעולות על מערכים:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

פלט:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## ראה גם
- [מדריך PHP על פונקציות](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: הדרך הנכונה - פונקציות](https://phptherightway.com/#functions)
- [למד על פונקציות חץ ב-PHP 7.4](https://stitcher.io/blog/short-closures-in-php)
