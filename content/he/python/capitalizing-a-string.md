---
title:                "כתיבת מחרוזת באותיות גדולות"
html_title:           "Python: כתיבת מחרוזת באותיות גדולות"
simple_title:         "כתיבת מחרוזת באותיות גדולות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
קפיטליזציה היא פעולה נפוצה בפיתוח תוכנה המשמשת לשנות את האותיות הראשונות של מחרוזות לאותיות גדולות. פעולה זו יכולה להיות שימושית כאשר נרצה להדגיש מילים מסוימות או להפוך את המחרוזת לנראית יותר מסודרת וקריאה.

## איך לעשות קפיטליזציה
```Python
# קוד דוגמה לשימוש בפונקציה capitalize
my_str = "hello world"
capitalized_str = my_str.capitalize()
print(capitalized_str)

# פלט:
Hello world
```

הפונקציה "capitalize" משנה את האות הראשונה של המחרוזת לאות גדולה. אם ישנן מחרוזות נוספות בתוך המחרוזת המקורית, הן לא ישתנו. כדי לקפוץ למצב כתיבה קפיטלית לעומת אותיות כתוב בכתיב קטן, ניתן להשתמש בפונקציה "upper":

```Python
# קוד דוגמה לשימוש בפונקציה upper
my_str = "hello world"
upper_str = my_str.upper()
print(upper_str)

# פלט:
HELLO WORLD
```

אם נרצה להיות מדוייקים יותר ולשנות את האותיות הראשונות של כל המילים במחרוזת לאותיות גדולות, נוכל להשתמש בפונקציה "title":

```Python
# קוד דוגמה לשימוש בפונקציה title
my_str = "hello world"
title_str = my_str.title()
print(title_str)

# פלט:
Hello World
```

## Deep Dive
הפונקציות "capitalize", "upper" ו-"title" הן רק חלקית מאופן הקפיטליזציה של מחרוזות בפייתון. ישנן פונקציות נוספות כמו "swapcase" שמשנות את כל האותיות הקטנות לאותיות גדולות וההפך. למידע נוסף על פונקציות אלו ועל איך להשתמש בהן, ניתן להתייעץ עם תיעוד פייתון הרשמי.

## ראו גם
- [תיעוד פייתון הרשמי](https://docs.python