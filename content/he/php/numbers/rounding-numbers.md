---
date: 2024-01-26 03:46:59.168798-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05DE\u05E2\u05D5 \u05E7\u05D9\u05D8\u05D5\u05DD \u05D4\u05E1\u05E4\
  \u05E8\u05D5\u05EA \u05D0\u05D7\u05E8\u05D9 \u05D4\u05E0\u05E7\u05D5\u05D3\u05D4\
  \ \u05DC\u05D3\u05D9\u05D5\u05E7 \u05E0\u05EA\u05D5\u05DF, \u05DC\u05E2\u05D9\u05EA\
  \u05D9\u05DD \u05DC\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05E9\u05DC\u05DE\u05D9\
  \u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\
  \u05DD \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05D7\u05D9\u05E9\u05D5\u05D1\
  \u05D9\u05DD, \u05DC\u05E9\u05E4\u05E8 \u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05DD\
  , \u05D0\u05D5 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05EA \u05D4\u05E4\u05DC\u05D8\
  \ \u05DC\u05D9\u05D3\u05D9\u05D3\u05D5\u05EA\u05D9\u2026"
lastmod: 2024-02-19 22:04:58.714760
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\
  \u05E9\u05DE\u05E2\u05D5 \u05E7\u05D9\u05D8\u05D5\u05DD \u05D4\u05E1\u05E4\u05E8\
  \u05D5\u05EA \u05D0\u05D7\u05E8\u05D9 \u05D4\u05E0\u05E7\u05D5\u05D3\u05D4 \u05DC\
  \u05D3\u05D9\u05D5\u05E7 \u05E0\u05EA\u05D5\u05DF, \u05DC\u05E2\u05D9\u05EA\u05D9\
  \u05DD \u05DC\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05E9\u05DC\u05DE\u05D9\u05DD\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05D7\u05D9\u05E9\u05D5\u05D1\u05D9\
  \u05DD, \u05DC\u05E9\u05E4\u05E8 \u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05DD, \u05D0\
  \u05D5 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05EA \u05D4\u05E4\u05DC\u05D8 \u05DC\
  \u05D9\u05D3\u05D9\u05D3\u05D5\u05EA\u05D9\u2026"
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים משמעו קיטום הספרות אחרי הנקודה לדיוק נתון, לעיתים למספרים שלמים. מתכנתים מעגלים כדי לפשט חישובים, לשפר ביצועים, או להפוך את הפלט לידידותי למשתמש.

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
