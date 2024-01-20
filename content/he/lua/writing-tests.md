---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות זה התהליך שבו מבצעים סקריפטים כדי לוודא שהקוד שלנו עובד כשורה. מתכנתים עושים זאת כדי למנוע באגים, לשפר את האיכות, ולהקל על תחזוקה בעתיד.

## איך לעשות:
בלואה, בדיקות ניתן לכתוב עם ספריות כמו LuaUnit. הנה דוגמה:

```Lua
local luaunit = require('luaunit')

function add(a, b)
    return a + b
end

function testAdd()
    luaunit.assertEquals(add(1, 1), 2)
end

os.exit( luaunit.LuaUnit.run() )
```
תוצאות הדוגמה:
```
..
Ran 1 test in 0.001 seconds, 1 success, 0 failures
```
## הצקה עמוקה
כתיבת בדיקות פיתחה בשנים האחרונות כחלק ממתודולוגיות פיתוח כמו TDD (Test-Driven Development). חלופות ל-LuaUnit הן Busted ו-Telescope. חשוב לזכור שתהליך הבדיקה צריך להיות אוטומטי ככל האפשר ולכלול בדיקות יחידה, כמו גם בדיקות אינטגרציה וקבלה.

## גם ראה
- LuaUnit: https://github.com/bluebird75/luaunit
- Busted: http://olivinelabs.com/busted/
- Telescope: https://github.com/norman/telescope

קיראו גם על TDD: https://en.wikipedia.org/wiki/Test-driven_development