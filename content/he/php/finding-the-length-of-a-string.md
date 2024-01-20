---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
התצוגה של אורך המחרוזת היא עיקרון מרכזי בתכנות שממדד את מספר התווים במחרוזת. זה נדרש לתכנות הקשרים, תיקוני בעיות ואלגוריתמים.

## כיצד ל:
הסבר כיצד למצוא את אורך מחרוזת ב-PHP מוצג כאן:

```PHP
<?php
$str = "ברוך הבא ל-PHP!";
echo strlen($str);
?>
```

כאשר תריץ את הקוד הזה, התוצאה תהיה 17 – כלולות הרווחים והסימנים.

## צלילה עמוקה
מונה אורך מחרוזת ב-PHP הוא הכלי הכי ישן שהשתמשו בו מתכנתים – כיצד להתמודד עם מחרוזות. קיימות אף טכניקות אחרות, כמו substr, אך strlen היא המהירה והיעילה ביותר. strlen מחזירה את מספר הבתים במחרוזת, לא תלוי באיזה גופן הוא משתמש. זה משפיע כאשר המחרוזת מכילה תווים חונקים.

## ראה גם
קישורים למקורות קשורים למדידת אורך מחרוזת:
1. [PHP: strlen](https://www.php.net/manual/en/function.strlen.php)
2. [PHP strlen() function](https://www.w3schools.com/php/func_string_strlen.asp)
3. [Determine the length of a string (PHP Cookbook)](https://www.oreilly.com/library/view/php-cookbook/1565926811/ch04s09.html)