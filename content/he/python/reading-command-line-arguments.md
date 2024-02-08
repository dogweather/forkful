---
title:                "קריאת פרמטרים משורת הפקודה"
aliases:
- he/python/reading-command-line-arguments.md
date:                  2024-01-20T17:57:20.668285-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה מאפשרת לתוכנית לקבל קלט גמיש ודינמי מהמשתמש. מתכנתים משתמשים בזה כדי להתאים את התוכניות שלהם לצרכים שונים ללא צורך לשנות את הקוד.

## איך עושים את זה:
ניתן לקרוא ארגומנטים משורת הפקודה בפייתון בעזרת המודול `sys`. כך זה נראה בפועל:
```python
import sys

# כדי להדפיס את כל הארגומנטים:
print(sys.argv)

# כדי להשתמש בארגומנט הראשון (לאחר שם התוכנית):
argument_one = sys.argv[1]
print(argument_one)
```
אם נריץ את הקוד עם הפקודה: `python my_script.py arg1 arg2`, הפלט יהיה:
```
['my_script.py', 'arg1', 'arg2']
arg1
```

## עומק הצלילה
ההיסטוריה של קליטת ארגומנטים משורת הפקודה היא עתיקה כמו ממשק השורת פקודה עצמו. בלינוקס ויוניקס, זה תמיד היה הדרך להעביר פרמטרים לתוכנית. בפייתון, `sys.argv` הוא השיטה הבסיסית, אבל ישנן שיטות נוספות וחזקות יותר כמו המודולים `argparse` ו`optparse` שמספקים יכולות ניתוח ארגומנטים מתקדמות יותר. דרך זאת כוללת בדיקות טיפוסים, ערכי ברירת מחדל, והודעות עזרה.

כאשר יוצרים כלים שמיועדים לשימוש מצד יותר ממשתמש בודד, בדרך כלל משתמשים ב`argparse` שכן הוא מספק ממשק פשוט ונגיש למשתמש.

## ראו גם:
- [מדריך התחלתי למודול argparse](https://docs.python.org/3/howto/argparse.html)
- [תיעוד המודול sys](https://docs.python.org/3/library/sys.html)
- [מאמר על קליטת ארגומנטים משורת הפקודה ב-Real Python](https://realpython.com/python-command-line-arguments/)
