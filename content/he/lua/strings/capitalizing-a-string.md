---
title:                "הגדלת אותיות במחרוזת"
aliases:
- /he/lua/capitalizing-a-string.md
date:                  2024-02-03T19:06:25.850026-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
