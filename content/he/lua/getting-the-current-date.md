---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה & למה?
השגת התאריך הנוכחי היא פעולה שבה המחשב מחזיר את התאריך והשעה של הרגע שבו הפעולה מתבצעת. מתכנתים משתמשים בזה מכיוון שלעיתים קרובות, נדרשים לעכל נתונים זמניים או ליישם כל פעולה שתלויה בזמן.

## כיצד לעשות:

ב-Lua, אנחנו מתמחים בפשטות, ולכן חוזרים לראשית בשימוש בפונקציה `os.date():

```Lua
os.execute("clear")

-- השג את התאריך היום
print("התאריך היום הוא: " .. os.date("%d/%m/%Y"))
```

הקוד התוך הלך בין ההערות.

## עומק יותר

על אף שהשימוש ב`os.date()` הוא הדרך המקובלת כיום ב-Lua לקבלת התאריך הנוכחי, בעבר השימוש היה מעט שונה ולא הציג תמיד את התאריך לפי שעון המערכת. בנוסף, קיימות גם ספריות של צד שלישי כגון LuaDate שמציעה תמיכה מורחבת, אך לעיתים יתר על המידה שרק מסבך את הדברים.
הפונקציה `os.date()`משתמשת בפונקציה המובנית של C `strftime()`, ולכן תהיה אחראית לוודא מגוון התמיכה של תבניות שיאפשר להמר תאריכים ושעות לפי התרחיש שלך.

## ראה גם:

1. התיעוד הרשמי של Lua: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
2. Stack Overflow threads on date and time in Lua: https://stackoverflow.com/questions/tagged/lua+date
3. LuaDate, a more feature-rich date library: https://github.com/Tieske/date