---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# מציאת אורך מחרוזת בפייתון

## מה זה ולמה?
מציאת אורך מחרוזת הוא פרוצדורה שמחשבת את כמות התווים במחרוזת. תכנתים משתמשים בו לשליטה וביצוע אודוד לוגי ברוב התסריטים והאפליקציות.

## כיצד:
```Python
str = "שלום, עולם!"
print(len(str))
```
הפלט:
```
12
```
## צלילה עמוקה:
1. הפקודה len() התוכנה של מאודול GNU C Library. מאז python 0.9.1 שוחרר בשנת 1991, הפקודה len() אפשרה למתכנתים לחשב את אורך מחרוזת באופן מהיר ויעיל.
2. חלופות אחרות כללות שימוש בלולאות או אלגוריתמים משלך לספור את התווים, אך len() הוא האופציה המומלצת מכיוון שזו הפקודה הרשמית של פייתון.
3. len() עובדת על-ידי החזרת הערך של משתנה מיוחד במחזור הפייתון, לכן היא ממומשת באופן מיידי ואין צורך לעבור דרך נתונים כדי למצוא את אורכו. 

## ראה גם:
עמוד המסמך של Python ל-func:`len`: https://docs.python.org/3/library/functions.html#len
עמוד השאלות הנפוצות של Python: https://docs.python.org/3/faq/programming.html#faq-programming