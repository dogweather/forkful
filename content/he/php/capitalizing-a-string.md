---
title:                "שיפור מחרוזת באמצעות אותיות גדולות"
html_title:           "PHP: שיפור מחרוזת באמצעות אותיות גדולות"
simple_title:         "שיפור מחרוזת באמצעות אותיות גדולות"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Hebrew Translation:

## מדוע
 למה לרוצה להשתמש ב-PHP כדי להפוך מחרוזת לגדולות כותרת?

## איך לעשות זאת
בהמשך תמצאו דוגמאות לקוד עם תוצאות בלוקי קוד "```PHP ... ```"

```PHP
$string = "שלום, עולם!";
echo strtoupper($string);
```
```
שלום, עולם!
```

Deep Dive: נעמוד בפרטים נוספים על הפעולה של פונקציית ה-"strtoupper" ב-PHP. הפעולה יכולה להיות מועילה במגוון רחב של מקרים, כגון יצירת כותרות גדולות או שינוי שפה במחרוזות.

בנוסף, השימוש בפעולה זו יכול לחסוך בהמון זמן ומאמץ מאחר וכותרות נמצאות תמיד בתוך קוד, ולא נדרש לכתוב שורות נוספות של קוד כדי להפוך אותן לגדולות.

## ראו גם

- [PHP עמוד הרשמי](https://www.php.net/)
- [הסבר על פעולת ה-"strtoupper" ב-PHP](https://www.geeksforgeeks.org/php-strtoupper-function/)
- [נוסחאות להפיכת מחרוזת לכותרת גדולה ב-PHP](https://www.w3schools.com/php/func_string_strtoupper.asp)