---
title:                "PHP: הפיכת מחרוזת לרבים"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מדוע
למה לעסוק בכתיבת פנקציה הבונה מחרוזת מכוונת?
כתיבה מחרוזת מכוונת יכולה לסייע במגוון רחב של מטרות בתכנות, כגון שינוי הכתב של מחרוזת, ייצוג נתונים מורכבים בצורה מתאימה יותר ועוד.

## כיצד לעשות זאת
נוכל להשתמש בפונקציה הכתובה מראש של PHP הנקראת strtoupper כדי להמיר את האותיות של מחרוזת לאותיות רישיות. הנה דוגמה:

```PHP
$string = "זהו דוגמה של מחרוזת";
echo strtoupper($string);
```

פלט:

**זהו דוגמה של מחרוזת**

כמו כן, ניתן לבנות פונקציה משלנו על ידי ניסוח קוד שעובד באותו אופן. לדוגמה:

```PHP
function capitalize($string) {
    return strtoupper($string);
}

$string = "זהו דוגמה של מחרוזת";
echo capitalize($string);
```

פלט:

**זהו דוגמה של מחרוזת**

## מקורות נוספים
כדי ללמוד עוד על כתיבת פונקציות מחרוזת מכוונת ב-PHP, ניתן לקרוא את המדריך הרשמי של PHP על [השתמש בפונקציות מחרוזת מכוונת](https://www.php.net/manual/en/function.strtoupper.php).
ניתן לקרוא גם על [פונקציות מחרוזת אחרות ב-PHP](https://www.php.net/manual/en/ref.strings.php) ולהתייעץ עם קהילת התכנות של PHP, [Stack Overflow](https://stackoverflow.com/).