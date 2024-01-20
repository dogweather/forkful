---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Lua: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
לראשית המילה של מחרוזת משמעה שמפוך את האות הראשונה של מילה לאות גדולה. מתכנתים משתמשים בזה לשיפור הבנת הקוד, בדרך כלל במקרים של קריאות API או שמות משתנים.

## איך:
```Lua
string1 = 'hebrew coding article'
capital_string = string1:gsub("^%l", string.upper)
print(capital_string)
```
תוצאה מדגמנת:
```Lua
Hebrew coding article
```
## צלילה עמוקה
1. התקנים מוגדרים בשפת Lua בגרסה 5.1, כאשר הוספו הפונקציות string.upper ו-string.gsub. אלה משמשות להמרת האות הראשונה של מילה לאות גדולה.
2. חלופה אחת היא להשתמש בספריה של third-party, כמו Luau, אך זה מתחייב לתלות נוספת.
3. הפקודה string.gsub מחפשת את כל האותיות הקטנות בתחילת המחרוזת וה- string.upper משנה אותן למקבילתם הגדולה.

## להסתכל גם
הפונקציה wiki page (Lua 5.1) - http://www.lua.org/manual/5.1/manual.html#pdf-string.gsub
לעזרה נוספת במאמרים על Lua - https://stackoverflow.com/questions/tagged/lua
ספרות המלצה שלו Lua - https://www.lua.org/docs.html