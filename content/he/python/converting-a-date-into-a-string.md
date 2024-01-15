---
title:                "המרת תאריך למחרוזת"
html_title:           "Python: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# למה
בעזרת פייתון ניתן להמיר תאריך למחרוזת כדי להציגו בצורה נוחה יותר למשתמשים. כך ניתן לייצר תאריך בפורמט שונה או להציגו בצורה של טבלה.

# איך לעשות זאת?
כדי להמיר תאריך למחרוזת בפייתון ישנם שני אפשרויות. הראשונה היא להשתמש בפונקציה str() להמרת התאריך למחרוזת. לדוגמה:
```python
date = datetime(2021, 7, 15)
print(str(date))
```
פלט: "2021-07-15"

האפשרות השנייה היא להשתמש בפונקציה strftime() כדי להמיר את התאריך למחרוזת בפורמט מותאם אישית. ישנם מספר אופציות לפורמט המחרוזת, כך שניתן להתאים את התאריך לצרכי המשתמש. לדוגמה:
```python
date = datetime(2021, 7, 15)
print(date.strftime('%d/%m/%Y'))
```
פלט: "15/07/2021"

# חפירה עמוקה
כאשר מפעילים את הפונקציה strftime() ישנם ברירות מחדל לפורמט, אך ניתן לשנות אותן לפי הצורך. לדוגמה, ניתן להשתמש בייצוג מספרי חלקים מהתאריך, כך שפוקס חלקים כמו השנה תוכל לייצר תאריך בפורמט ייחודי למשתמשים שלו. ניתן גם להשתמש בפונקציות נוספות כמו localtime() ו- strftime() כדי להציג את התאריך בזמן נוכחי של מיקום מסוים. כמו כן, קיימים ספריות נוספות כמו datetime ו- time לניהול מבני התאריך והזמן בפייתון.

# ראו גם
- [תיעוד על המרת תאריך למחרוזת בפייתון](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Tutorialspoint: How to Convert Date to String in Python](https://www.tutorialspoint.com/python3/time_strftime.htm)
- [W3Schools: Python Datetime](https://www.w3schools.com/python/python_datetime.asp)