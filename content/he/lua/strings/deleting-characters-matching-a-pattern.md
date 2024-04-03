---
date: 2024-01-20 17:43:05.997988-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.524841-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

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
