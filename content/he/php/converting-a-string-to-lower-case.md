---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא פעולה שמשנה את כל האותיות הגדולות במחרוזת לאותיות קטנות. מתכנתים מבצעים זאת לרוב כדי להשוות בין מחרוזות בצורה יעילה ונכונה, ללא התחשבות באותיות גדולות או קטנות.

## איך לעשות:
הנה דוגמאות לקוד ולתוצאות שהם מחזירים.

```PHP
$str = "Hello, World!";
$lowercase = strtolower($str);
echo $lowercase; // outputs: hello, world!
```

## היכנסו לעומק:
מערכת הפונקציות ב-PHP להמרת מחרוזת לאותיות קטנות התחילה בגרסה 4 של השפה. בגרסאות המאוחרות יותר המערכת החלה לתמוך גם באותיות גדולות וקטנות בשפות שונות, לא רק באנגלית.

חלופות ל-`strtolower()` כוללות הפונקציה `mb_strtolower()`, שתומכת בהמרת אותיות גדולות לקטנות במבחר של כמה קידודים שונים.

המימוש של הפונקציה `strtolower()` מבוסס על מערך של אותיות גדולות וקטנות, והיא משווה כל אות במחרוזת המתקבלת לאות מתאימה במערך.

## ראו גם:
למידה עמוקה יותר:
1. [המכניקה של המרת מחרוזת לאותיות קטנות](https://www.php.net/manual/en/function.strtolower.php)
2. [המרת מחרוזת לאותיות גדולות ב-PHP](https://www.php.net/manual/en/function.strtoupper.php)
3. [עיבוד מחרוזות ב-PHP](https://www.php.net/manual/en/book.strings.php)