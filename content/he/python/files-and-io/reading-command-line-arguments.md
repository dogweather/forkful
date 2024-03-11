---
date: 2024-01-20 17:57:20.668285-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \ \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05D2\u05DE\u05D9\u05E9 \u05D5\u05D3\
  \u05D9\u05E0\u05DE\u05D9 \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D0\u05EA\
  \ \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05E9\u05DC\u05D4\u05DD \u05DC\
  \u05E6\u05E8\u05DB\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD \u05DC\u05DC\u05D0\
  \ \u05E6\u05D5\u05E8\u05DA \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA\u2026"
lastmod: '2024-03-11T00:14:12.077519-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \ \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05D2\u05DE\u05D9\u05E9 \u05D5\u05D3\
  \u05D9\u05E0\u05DE\u05D9 \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D0\u05EA\
  \ \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05E9\u05DC\u05D4\u05DD \u05DC\
  \u05E6\u05E8\u05DB\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD \u05DC\u05DC\u05D0\
  \ \u05E6\u05D5\u05E8\u05DA \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
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
