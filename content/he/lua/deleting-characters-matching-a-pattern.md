---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:43:05.997988-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת תווים שמתאימים לדפוס בודדים היא פעולה נפוצה בתכנות שבה מנקים מחרוזות מתווים לא רצויים. תכנתים עושים זאת כדי לנקות קלט מהמשתמש, לפרמט נתונים ולהסיר תווים מיותרים או לא בטוחים.

## איך לעשות:
```Lua
local myString = "Hello, World! 1234"

-- נמחק את כל הספרות
local cleanedString = myString:gsub("%d", "")
print(cleanedString)  --> Hello, World! 

-- נמחק אותיות ורווחים
cleanedString = myString:gsub("[%a ]", "")
print(cleanedString)  --> ,!1234
```

## צלילה לעומק
הטכניקה להסרת תווים עובדת בעזרת ביטויים רגולריים, שהם שיטה חזקה וגמישה לחיפוש תבניות בטקסט, התקנה בשנת 1950 ונטמעה ברבות משפות התכנות. בלואה, שימוש ב`:gsub()` היא הדרך להפעיל ביטויים רגולריים. ישנם גם כלים אחרים כמו `tr` ב-UNIX או ספריות צד שלישי בשפות אחרות, אך `gsub` היא פשוטה ויעילה למחיקת תווים לפי תבנית.

## לראות גם
- Lua 5.4 Reference Manual: `gsub`: https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub
- למד יותר על ביטויים רגולריים: https://www.regular-expressions.info/
- קורס וידאו שמסביר על ביטויים רגולריים בלואה: https://www.youtube.com/watch?v=7eKtplpOJ1I
