---
date: 2024-01-26 04:16:42.762650-07:00
description: "REPL \u05DE\u05E6\u05D9\u05D9\u05DF Read-Eval-Print Loop, \u05E1\u05D1\
  \u05D9\u05D1\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\
  \u05EA \u05E9\u05D1\u05D4 \u05E0\u05D9\u05EA\u05DF \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05E7\u05D5\u05D3 \u05D1\u05DE\u05D4\u05D9\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D1\u05D4 \u05E9\u05D9\
  \u05DE\u05D5\u05E9 \u05DC\u05E0\u05D9\u05E1\u05D5\u05D9, \u05D0\u05D9\u05EA\u05D5\
  \u05E8 \u05D1\u05D0\u05D2\u05D9\u05DD, \u05D5\u05DC\u05DC\u05DE\u05D5\u05D3 \u05D0\
  \u05EA \u05D4\u05EA\u05D9\u05E7\u05D5\u05E0\u05D9\u05DD \u05D4\u05D9\u05D9\u05D7\
  \u05D5\u05D3\u05D9\u05D9\u05DD\u2026"
lastmod: 2024-02-19 22:04:58.801144
model: gpt-4-0125-preview
summary: "REPL \u05DE\u05E6\u05D9\u05D9\u05DF Read-Eval-Print Loop, \u05E1\u05D1\u05D9\
  \u05D1\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA\
  \ \u05E9\u05D1\u05D4 \u05E0\u05D9\u05EA\u05DF \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E7\
  \u05D5\u05D3 \u05D1\u05DE\u05D4\u05D9\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D1\u05D4 \u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05DC\u05E0\u05D9\u05E1\u05D5\u05D9, \u05D0\u05D9\u05EA\u05D5\u05E8\
  \ \u05D1\u05D0\u05D2\u05D9\u05DD, \u05D5\u05DC\u05DC\u05DE\u05D5\u05D3 \u05D0\u05EA\
  \ \u05D4\u05EA\u05D9\u05E7\u05D5\u05E0\u05D9\u05DD \u05D4\u05D9\u05D9\u05D7\u05D5\
  \u05D3\u05D9\u05D9\u05DD\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?
REPL מציין Read-Eval-Print Loop, סביבה אינטראקטיבית שבה ניתן לבדוק קוד במהירות. מתכנתים עושים בה שימוש לניסוי, איתור באגים, וללמוד את התיקונים הייחודיים של שפה.

## איך לעשות:
כדי לקפוץ ל-REPL של Lua, פשוט הקלד `lua` בטרמינל שלך. הנה דוגמא לסשן:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
בסשן, אנחנו מצהירים על משתנה, בוצעת חשבון בסיסי, מניפולציה של טבלה, ולולאה דרך הפריטים שלה.

## צלילה עמוקה
המהות הקלילה של Lua הופכת את ה-REPL שלה לאידיאלי לפרוטוטיפינג. היא קיימת מאז הופעת Lua בתחילת שנות ה-90, והושראה מקודמות אינטראקטיביות לשפות כמו Lisp. חלופות בשפות אחרות כוללות את `irb` עבור Ruby ו-`python` עבור Python, כל אחת עם ערכת תכונות משלה. REPL של Lua הוא מינימליסטי; לכן, ייתכן שיחסרו בו תכונות מתקדמות שנמצאות באחרות, כמו כלים מתקדמים לאיתור בעיות. לחוויה מורחבת יותר, כלים כמו ZeroBrane Studio או LuaDist's LuaRocks מציעים יותר מ-REPL הבסיסי.

## ראה גם
- [Lua 5.4 Manual Manual - The Standalone Lua Interpreter](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
