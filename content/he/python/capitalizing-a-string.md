---
title:                "הפיכת מחרוזת לאותיות ראשיות"
html_title:           "Python: הפיכת מחרוזת לאותיות ראשיות"
simple_title:         "הפיכת מחרוזת לאותיות ראשיות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
הגדלת האות הראשונה במחרוזת משתמשי Python ממירה את האות הראשונה במחרוזת לאות גדולה, וכל האותיות האחרות לאוֹתִיוֹת קְטָנוֹת. התכנתים עושים את זה, פשוט מכיוון שלעיתים נדרש לנקוט בקליטה ממה שהמשתמש הקליד ולהציג אותו באופן מנומס ביותר.

## איך לעשות:
הנה דוגמה פשוטה של השימוש בפונקציה המובנית `capitalize()` ב-Python.

```Python
s = 'hello world'
print(s.capitalize())
```
הפלט של התוכנית יהיה:
```Python
'Hello world'
```
## עומק מורה:
עם רקע מינימאלי ב-Python, ראינו למה משתמשים בפונקציה `capitalize()`. (1) בהקשר ההיסטורי, זו הייתה אחת מהפונקציות הראשונות שהוצעו לעיבוד מחרוזות בשפת Python. (2) ישנם חלופות ל-'capitalize()', כמו `'upper()'`, שממיר את כל האותיות לאותיות גדולות, או `'title()'`, שממיר את האות הראשונה של כל מילה לאות גדולה. (3) לגבי הפרטים של היישום, `capitalize()` ממיר את כל האותיות הראשונות של מילה לאותיות גדולות ואת שאר האותיות לאותיות קטנות.

## ראה גם:
1. מתיחס למסמכי Python על `capitalize()`: 
[Python Docs: String Methods](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
2. דיסקוסיה על `capitalize()` vs `title()`: 
[Discussion on StackOverflow](https://stackoverflow.com/questions/1549641/how-can-i-capitalize-the-first-letter-of-each-word-in-a-string)
3. לומד Python? תנסו אלו קורסים בקישור:
[Python Courses](https://www.coursera.org/courses?query=python)