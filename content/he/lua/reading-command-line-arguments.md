---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים מהשורה של הפקודות היא התהליך שבו תוכנית מקבלת מספרים, מחרוזות, או אפילו אובייקטים מבין הנתונים שהוזנו למערכת שם הפקודות. מתכנתים מספרים בטכניקה הזו כדי להפוך את התוכנית שלהם לנתונים גמישים ונמתחים יותר.

## איך הופקים:
כאן הקוד בלועזית של הדוגמא:

```Lua
-- תוכנית Lua פשוטה לדוגמא של קריאת ארגומנטים מהשורת הפקודות.
local args = {...}
print("Hello, " .. args[1])
```
אתה מפעיל את התוכנית כך:

```sh
lua hello.lua World
```
וזה ידפיס:

```sh
Hello, World
```

## עמיק יותר:
קריאת ארגומנטים מהשורה של הפקודות היא מתכנתית בסיסית שנוצרה בשפות התכנות הראשונות. בביצועים מתקדמים יותר, אתה יכול להשתמש בספריות גדולות יותר כמו argparse של Python. אך ב Lua, מגיע עם פונקציונליות פשוטה יכולה לעשות את העבודה בדרך כלל.

הארגומנטים מהשורה של הפקודות ניתנים לתוכנית לפני התחלת הריצה. לשפת Lua מערך מיוחד בשם arg המכיל את כל הארגומנטים שהועברו לסקריפט, לגישה נוחה.

## ראה גם:
- [תיעוד Offical Lua](http://www.lua.org/manual/5.4/manual.html#pdf-arg)
- [Lua בקנה המידה](http://luatut.com/crash_course.html)
- [אינטראקציה עם שורת הפקודה](http://lua-users.org/wiki/CommandLineParsing)
- [העמוד של Lua ב-Wikibooks](https://he.wikibooks.org/wiki/%D7%A2%D7%96%D7%A8%D7%94:%D7%AA%D7%99%D7%9B%D7%95%D7%A0%D7%95%D7%AA_%D7%94%D7%A1%D7%A4%D7%A8_%D7%94%D7%97%D7%93%D7%A9/%D7%9C%D7%95%D7%A2%D7%94)