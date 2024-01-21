---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:51:52.514123-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת זה תהליך שבו אתה משלב משתנים וביטויים בתוך מחרוזת. תוכניתנים עושים זאת כדי ליצור מחרוזות דינמיות שמשתנות בהתאם למידע או למצבים שונים.

## איך לעשות:
בלואה, לא ממש קיימת תמיכה ישירה לאינטרפולציה, אבל אפשר להשיג תוצאה דומה בקלות עם `string.format` או ביטויים מצורפים:

```lua
local name = "דני"
local age = 29

-- שיטה 1: באמצעות string.format
local greeting = string.format("שלום, קוראים לי %s ואני בן %d.", name, age)
print(greeting) -- שלום, קוראים לי דני ואני בן 29.

-- שיטה 2: באמצעות הצמדת מחרוזות
local greeting2 = "שלום, קוראים לי " .. name .. " ואני בן " .. age .. "."
print(greeting2) -- שלום, קוראים לי דני ואני בן 29.
```

## עיון מעמיק
האינטרפולציה של מחרוזות בפייתון או ג'אווהסקריפט פשוטה יותר מאשר בלואה, בעיקר בגלל התמיכה הישירה שלהן בתכונה זו. אך גם בלואה תוכל לעשות את העבודה בקלות בעזרת הפונקציה `string.format`, שמבוססת על הסימון של פונקציה דומה מ-C. זו עשויה להראות קצת פחות נקייה, אבל היא חזקה מאוד ונותנת לך שליטה גדולה בפורמט הסופי של המחרוזת.

## לקרוא גם
- [מדריך ל-`string.format`](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [תיעוד לואה רשמי](https://www.lua.org/manual/5.4/)
- [סקירת בסיסי השפה לואה](https://learnxinyminutes.com/docs/lua/)