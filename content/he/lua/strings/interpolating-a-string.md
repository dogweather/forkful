---
aliases:
- /he/lua/interpolating-a-string/
date: 2024-01-20 17:51:52.514123-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05EA\u05D4\u05DC\u05D9\
  \u05DA \u05E9\u05D1\u05D5 \u05D0\u05EA\u05D4 \u05DE\u05E9\u05DC\u05D1 \u05DE\u05E9\
  \u05EA\u05E0\u05D9\u05DD \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D3\u05D9\u05E0\u05DE\u05D9\u05D5\u05EA \u05E9\u05DE\u05E9\u05EA\
  \u05E0\u05D5\u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05DE\u05D9\u05D3\u05E2\
  \ \u05D0\u05D5 \u05DC\u05DE\u05E6\u05D1\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD\
  ."
lastmod: 2024-02-18 23:08:52.962162
model: gpt-4-1106-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05EA\u05D4\u05DC\u05D9\
  \u05DA \u05E9\u05D1\u05D5 \u05D0\u05EA\u05D4 \u05DE\u05E9\u05DC\u05D1 \u05DE\u05E9\
  \u05EA\u05E0\u05D9\u05DD \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D3\u05D9\u05E0\u05DE\u05D9\u05D5\u05EA \u05E9\u05DE\u05E9\u05EA\
  \u05E0\u05D5\u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05DE\u05D9\u05D3\u05E2\
  \ \u05D0\u05D5 \u05DC\u05DE\u05E6\u05D1\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD\
  ."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
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
