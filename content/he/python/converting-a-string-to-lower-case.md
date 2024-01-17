---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Python: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לתווים נמוכים היא תהליך שתכניות מחשב עושות כדי להפוך את כל האותיות במחרוזת לאותיות גדולות לאותיות קטנות. זה עשוי להשתמש לכמה מטרות שונות כגון ייצוג בינתוח טקסט או ועבודת דטה.

## איך ל:
כדי להמיר מחרוזת לתווים נמוכים בפייתון, שימוש בפונקציה `lower()` על המחרוזת. לדוגמה:

```Python
my_string = "HELLO WORLD"
print(my_string.lower())
```
תוצאה: `hello world`

אפשר להשתמש גם במתודה `casefold()` כדי לקבל תוצאה דומה.

## שכיח עומק
המרת מחרוזת לתווים נמוכים היא תהליך נפוץ בכמה שפות תכנות ונעשה בעיקר לצורך ייצוג ועבודה עם טקסט. קיימות גם מתודות אחרות כמו `upper()` שממירה לאותיות גדולות ו `capitalize()` שממירה רק את האות הראשונה במחרוזת לאות גדולה. ההשפעה היסטורית של תהליך זה קשורה לשימוש בשפת תכנות בלהנדה כדי להעביר טקסט לתווים נמוכים מה שיצר את הפונקציה `tolower()`.

## ראה גם:
- [מדריך Python רשמי על `lower()`](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [תיעוד ל `casefold()` מתוך המדריך הרשמי של פייתון](https://docs.python.org/3/library/stdtypes.html#str.casefold)
- [מדריך על פונקציות מחרוזת בפייתון](https://www.w3schools.com/python/python_strings.asp)