---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים התואמים לדפוס היא תהליך שבו מסירים תווים מחרוזת תוך שמירה רק על התווים שאינם תואמים לדפוס מסוים. מתכנתים עושים זאת כדי לנקות או לחדד את הנתונים שלהם.

## איך מבצעים:
הנה קטע קוד שמדגים איך אפשר למחוק תווים שתואמים לדפוס ב-PHP:

```PHP
<?php
$string = "Hello, בעולם!";
$pattern = '/[^א-ת ]*/u';
$cleanedString = preg_replace($pattern, '', $string);
echo $cleanedString;
?>
```

הפלט של הקוד הוא: 
```
בעולם
```

## בחיק המידע:
מאז ומתמיד, מתכנתים חיפשו דרכים לתמחזר חרוזות. כאשר PHP נוצרה בשנת 1994, לקוחה היתה בחסר בכלים לטיהור חרוזות. אך עם הזמן, פונקציות כמו preg_replace הוסיפו יכולת לרוב המשימות. בדר"כ, מחיקת תווים ע"פ דפוס הוא הפתרון הפשוט ביותר, אך ישנם אלטרנטיבות, כמו להשתמש בפונקציה str_replace או בפונקציה strtr. 

## ראה גם:
- ועידת PHP: https://www.php.net/manual/en/function.preg-replace.php
- ויקי PHP - מערכת של ביטויים רגילים: https://en.wikipedia.org/wiki/Regular_expression
- דוקומנטציה על preg_replace: https://www.php.net/manual/en/function.preg-replace.php
- str_replace: https://www.php.net/manual/en/function.str-replace.php
- strtr: https://www.php.net/manual/en/function.strtr.php