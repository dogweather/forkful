---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:58:28.280431-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפה של טקסט הם פעולות נפוצות שמאפשרות לנו למצוא מחרוזות טקסט ספציפיות ולהחליפן באחרות. פרוגרמרים נעזרים בהן כדי לעדכן קוד, לתקן שגיאות או לשנות נתונים באופן אוטומטי.

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