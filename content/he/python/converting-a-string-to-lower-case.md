---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות ב-Python הוא מטלה פשוטה שמבצעים בעזרת הפונקציה `lower()`. מתכנתים משתמשים בשיטה זו כדי לספר בין מחרוזות באופן שאינו תלוי ברישיות האותיות.

## כיצד ל:
כאן נראה את הדרך לשנות מחרוזת לאותיות קטנות ב-Python:
```Python
original_string = "Hello, Python!"
lowercase_string = original_string.lower()
print(lowercase_string)
```

בקוד הזה, החזרה הפלט תהיה:
```
hello, python!
```

## צלילה עמוקה
שימוש בפונקציה `lower()` ב-Python אמנם פשוט, אך ישנם דברים מעניינים לדעת עליו. `'Python'` זכה תחילה לפונקציה זו בשביל לסייע בהשוואת מחרוזות שאינן תלויות ברישיות. קיימות גם חלופות ל-`lower()`, כמו `capitalize()` ו-`title()`, אך אלו לא מבצעות את אותו הפעולה.

## ראו גם
חפשו מקורות נוספים על עבודה עם מחרוזות ב-Python:
1. הפונקציה lower() בתיעוד הרשמי של Python [כאן](https://docs.python.org/3/library/stdtypes.html#str.lower).
2. מדריך מקיף על מחרודת ב-Python באתר [w3schools](https://www.w3schools.com/python/python_strings.asp).