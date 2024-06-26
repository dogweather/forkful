---
date: 2024-01-26 03:46:59.168798-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: PHP \u05DE\u05E6\u05D9\
  \u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05D3\u05E8\u05DB\u05D9\u05DD \u05DC\u05E2\
  \u05D2\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD: `round()`, `ceil()`, \u05D5-\
  \ `floor()`. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA \u05D4\u05DF \u05E2\u05D5\u05D1\
  \u05D3\u05D5\u05EA."
lastmod: '2024-03-13T22:44:39.472426-06:00'
model: gpt-4-0125-preview
summary: "PHP \u05DE\u05E6\u05D9\u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05D3\u05E8\
  \u05DB\u05D9\u05DD \u05DC\u05E2\u05D2\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  ."
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## איך לעשות:
PHP מציעה מספר דרכים לעגל מספרים: `round()`, `ceil()`, ו- `floor()`. הנה איך הן עובדות:

```php
echo round(3.14159);   // מחזיר 3
echo round(3.14159, 2); // מחזיר 3.14

echo ceil(3.14159);    // מחזיר 4, תמיד מעגל למעלה

echo floor(3.14159);   // מחזיר 3, תמיד מעגל למטה
```

## עיון מעמיק
עיגול מספרים היה חיוני במתמטיקה ובחישוב מאז ימי קדם כדי להתמודד עם עשרוניות אינסופיות בלתי פרקטיות. ב-PHP, `round()` יכול לקבל פרמטר דיוק ומצב, המשפיעים על התנהגותו - `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN` וכו', מגדירים איך יתנהג כאשר הוא פוגש בסיטואציה של ".5". הדיוק הוא קריטי ביישומים פיננסיים שבהם העיגול עשוי להיות מוסדר על ידי חוק, המשפיע על איך `round()` מיושם בקוד.

אלטרנטיבות לפונקציות הטבעיות כוללות שיטות עיגול מותאמות אישית או פונקציות BC Math לחישוב אריתמטי עם דיוק שרירותי, המתאימים לתרחישים הדורשים יותר שליטה או התמודדות עם מספרים גדולים מאוד שבהם הדיוק הטבעי עלול להיכשל.

## ראו גם
גלו עוד במדריך PHP:
- [פונקציית `round` של PHP](https://php.net/manual/en/function.round.php)
- [פונקציית `ceil` של PHP](https://php.net/manual/en/function.ceil.php)
- [פונקציית `floor` של PHP](https://php.net/manual/en/function.floor.php)
- [BC Math לחישוב אריתמטי בדיוק שרירותי](https://php.net/manual/en/book.bc.php)
