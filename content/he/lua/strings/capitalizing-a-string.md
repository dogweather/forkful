---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:25.850026-07:00
description: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D5\u05E0\u05D5\
  \u05EA \u05D2\u05D3\u05D5\u05DC\u05D5\u05EA \u05DB\u05D5\u05DC\u05DC\u05EA \u05E9\
  \u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\
  \u05D5\u05DF \u05E9\u05DC \u05DB\u05DC \u05DE\u05D9\u05DC\u05D4 \u05D1\u05DE\u05E9\
  \u05E4\u05D8 \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\
  \u05DA \u05DB\u05D3\u05D9 \u05D5\u05D3\u05D0\u05D9\u05D5\u05EA \u05E9\u05D4\u05E9\
  \u05D0\u05E8 \u05D9\u05D4\u05D9\u05D5 \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D5\u05EA. \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05D6\u05D5 \u05E0\
  \u05E4\u05D5\u05E6\u05D4 \u05DC\u05E2\u05D9\u05E6\u05D5\u05D1\u2026"
lastmod: '2024-03-13T22:44:39.522788-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D5\u05E0\u05D5\u05EA\
  \ \u05D2\u05D3\u05D5\u05DC\u05D5\u05EA \u05DB\u05D5\u05DC\u05DC\u05EA \u05E9\u05D9\
  \u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\
  \u05DF \u05E9\u05DC \u05DB\u05DC \u05DE\u05D9\u05DC\u05D4 \u05D1\u05DE\u05E9\u05E4\
  \u05D8 \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\u05DA\
  \ \u05DB\u05D3\u05D9 \u05D5\u05D3\u05D0\u05D9\u05D5\u05EA \u05E9\u05D4\u05E9\u05D0\
  \u05E8 \u05D9\u05D4\u05D9\u05D5 \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\
  \u05E0\u05D5\u05EA."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## מה ולמה?
הפיכת מחרוזת לאותיות ראשונות גדולות כוללת שינוי של התו הראשון של כל מילה במשפט לאות גדולה, תוך כדי ודאיות שהשאר יהיו אותיות קטנות. טכניקה זו נפוצה לעיצוב טקסט לפלט מקצועי או קריא יותר, כמו הכנת כותרות או קלט משתמש לתצוגה.

## איך לעשות:
לואה אינה מכילה פונקציה מובנית להפיכת מחרוזות לאותיות ראשונות גדולות, אך ניתן לבצע זאת בקלות באמצעות פונקציות מניפולציה בסיסיות של מחרוזות. הנה פונקציה פשוטה להפיכת האות הראשונה של מילה בודדת לאות גדולה:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- פלט: Hello
```

להפוך כל מילה במשפט לאות ראשונה גדולה, ניתן לפצל את המשפט למילים, להפוך כל אחת מהן לאות ראשונה גדולה, ולאחר מכן לחבר אותן מחדש:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- פלט: Hello World From Lua
```

אם אתה עובד על פרויקט שבו הביצועים קריטיים ואתה מוצא את עצמך זקוק ליכולות מניפולציה מתקדמות יותר של מחרוזות, שקול להשתמש בספריית צד שלישי כמו `Penlight`. Penlight מגדילה את לואה עם פונקציות טיפול במחרוזות גמישות יותר, בין השאר:

```lua
-- בהנחה ש-Penlight מותקנת:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- פלט: Hello lua users

-- שים לב: פונקציית ה-capitalized של Penlight מפוכה רק את המילה הראשונה.
-- להפוכת כל מילה, היית צריך לבצע פיתרון מותאם אישית או לחפש בספריות אחרות.
```
