---
title:                "מציאת אורך של מחרוזת"
html_title:           "PHP: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

איך מציאת אורך של מחרוזת עוזרת למתכנתים? כאשר אנחנו נעבוד עם מחרוזות רבות בתוכניות שלנו, לפעמים נצטרך להכיר את אורךן. האורך של מחרוזת מציין את מספר התווים שבה, וזה יכול להיות חשוב בהרבה תהליכים שבהם אנחנו עובדים עם טקסטים. המימוש של מציאת אורך של מחרוזת ב-PHP הוא פשוט ויעיל, כך שזה יכול להיות מאוד שימושי למתכנתים.

## איך לעשות זאת:

```PHP
// הגדרת מחרוזת עם מספר תווים:
$text = "ברוכים הבאים לכתבה על מציאת אורך של מחרוזת ב-PHP";

// הדפסת אורך המחרוזת עם הפונקציה strlen:
echo strlen($text);

// פלט: 46
```

## מעמקים:

מציאת אורך של מחרוזת היא כלי בסיסי ב-PHP ובשפות תכנות אחרות גם. הפונקציה strlen קיימת כבר מאז PHP 4 והיא נמצאת כעת גם בגרסאות האחרונות והמתקדמות ביותר. ישנן גם פונקציות דומות בשפות תכנות אחרות, כגון len ב-Java ו-strlen ב-C.

## ראו גם:

- [מסמך הרחבה PHP הרשמי על למדידת אורך של מחרוזת](https://www.php.net/manual/en/function.strlen.php)
- [השוואת בין שני תווים במחרוזת ב-PHP אורך למספר התווים](https://www.php.net/manual/en/function.strcmp.php)
- [רשימת פקודות PHP הנפוצות](https://www.php.net/manual/en/language.basic-syntax.php)