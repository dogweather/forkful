---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases: - /he/lua/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:42.762650-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/using-an-interactive-shell-repl.md"
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
