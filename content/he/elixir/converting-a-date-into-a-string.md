---
title:                "המרת תאריך למחרוזת"
html_title:           "Elixir: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה
אחד המשימות הנפוצות בתכנות היא קידוד תאריכים למחרוזת כדי שיוצגו בצורה נוחה וקריאה למשתמשים. כך שהמחשב יוכל להתאים בעצמו את התאריך לפי המדינה והפורמט המועדף על המשתמשים.

## כיצד לעשות זאת
הדרך היעילה ביותר להמרת תאריך למחרוזת באליקסיר היא להשתמש בפונקציה ```Elixir 
Calendar.format/3```. ניתן להעביר לה תאריך ומחרוזת של פורמט התצוגה הרצוי. לדוגמה:

```Elixir
iex> date = Date.utc_today()
~D[2020-07-14]
iex> Calendar.format(date, "{0} {D} of {M}, {Y}", :long)
"Tuesday 14th of July, 2020"
```

ניתן גם להשתמש בפונקציות נוספות כמו ```Elixir 
Date.to_string/2``` ו-```Elixir Date.to_iso8601/2``` כדי לקבל מחרוזת בפורמטים מוכרים יותר.

## מעמקים
לבניית פורמט התצוגה לתאריך יש מספר אפשרויות, החל מפורמטים מוכרים כמו "DD/MM/YYYY" ועד לפורמטים מותאמים אישית לכל משתמש. בנוסף, ניתן להשתמש בפונקציות כמו ```Elixir Date.from_iso8601/2``` כדי להמיר מחרוזת לתאריך בפורמט ISO 8601 ולהתאים אותו לפי המדינה והתצוגה הנדרשת.

## ראה גם
- [העברת תאריך למחרוזת באליקסיר](https://elixir-lang.org/getting-started/datetime.html#format) 
- [רשימת פונקציות קידוד תאריכים באליקסיר](https://hexdocs.pm/elixir/Date.html)