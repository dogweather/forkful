---
date: 2024-01-20 17:58:28.280431-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D4 `gsub` \u05D1\u05DC\u05D5\u05D0\u05D4 \u05E0\u05D5\
  \u05E2\u05D3\u05D4 \u05DC\u05D1\u05E6\u05E2 \u05D4\u05D7\u05DC\u05E4\u05D5\u05EA\
  \ \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D8\u05E7\u05E1\u05D8\
  \ \u05D5\u05D4\u05D9\u05D0 \u05DE\u05D2\u05D9\u05E2\u05D4 \u05DE\u05D4\u05DE\u05D9\
  \u05DC\u05D4 \"global substitution\". \u05DE\u05D1\u05E0\u05D4 \u05D4\u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D4 \u05D4\u05D5\u05D0: `string.gsub(s, pattern,\u2026"
lastmod: '2024-04-05T21:53:40.669565-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `gsub` \u05D1\u05DC\u05D5\
  \u05D0\u05D4 \u05E0\u05D5\u05E2\u05D3\u05D4 \u05DC\u05D1\u05E6\u05E2 \u05D4\u05D7\
  \u05DC\u05E4\u05D5\u05EA \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D5\u05D4\u05D9\u05D0 \u05DE\u05D2\u05D9\u05E2\u05D4\
  \ \u05DE\u05D4\u05DE\u05D9\u05DC\u05D4 \"global substitution\"."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## איך לעשות:
```Lua
-- חיפוש והחלפה בסיסיים באמצעות gsub
local original_text = "שוקולד מתוק מאוד מתוק"
local text_to_find = "מתוק"
local replacement = "מר"

local replaced_text = string.gsub(original_text, text_to_find, replacement)
print(replaced_text)  -- תוצאה: שוקולד מתוק מר מר
```

```Lua
-- חיפוש עם פטרנים (Patterns)
local pattern_text = "ילד 1, ילדה 2, ילד 3"
local pattern = "ילד(ה?) %d"

local matches = {string.gmatch(pattern_text, pattern)}

for i, match in ipairs(matches) do
   print("מצאתי: " .. match)
end
-- תוצאה:
-- מצאתי: ילד 1
-- מצאתי: ילדה 2
-- מצאתי: ילד 3
```

## עיון יותר עמוק:
הפונקציה `gsub` בלואה נועדה לבצע החלפות של מחרוזות טקסט והיא מגיעה מהמילה "global substitution". מבנה הפונקציה הוא: `string.gsub(s, pattern, replace, [n])`. `s` היא המחרוזת המקורית, `pattern` הוא הטקסט או הביטוי שאנחנו מחפשים, `replace` הוא הטקסט להחלפה, ואופציונלית `n` מגביל את מספר ההחלפות.

פטרנים בלואה מאפשרים חיפוש יותר מתקדם עם מנגנונים דומים לביטויים רגולריים. למשל, הסימן `%d` מייצג כל ספרה. 

חשוב להבין את המגבלות: ביטויים רגולריים שלמים אינם נתמכים בלואה אלא רק פטרנים מסוימים. למשל, לא תמצאו בלואה `+` או `?` כפי שהם מופיעים בביטויים רגולריים.

## ראו גם:
- מדריך למחרוזות בלואה: [Programming in Lua: Strings](https://www.lua.org/pil/20.html)
- תיעוד של הפונקציה `gsub`: [Lua 5.4 Reference Manual: gsub](https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub)
- תיעוד על פטרנים בלואה: [Lua 5.4 Pattern Matching](https://www.lua.org/manual/5.4/manual.html#6.4.1)
