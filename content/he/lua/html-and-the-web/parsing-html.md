---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:57.762624-07:00
description: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\
  \u05D4\u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML, \u05DE\
  \u05D4 \u05E9\u05D7\u05E9\u05D5\u05D1 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E1\u05E8\
  \u05D9\u05E7\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05E0\u05D9\u05EA\
  \u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05DE\u05E9\u05D9\u05DE\
  \u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D5\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E1\u05D5\u05E3, \u05DC\u05E0\u05EA\u05D7\
  \ \u05D0\u05D5\u2026"
lastmod: 2024-02-19 22:04:58.794363
model: gpt-4-0125-preview
summary: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\
  \u05D4\u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML, \u05DE\
  \u05D4 \u05E9\u05D7\u05E9\u05D5\u05D1 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E1\u05E8\
  \u05D9\u05E7\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05E0\u05D9\u05EA\
  \u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05DE\u05E9\u05D9\u05DE\
  \u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D5\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E1\u05D5\u05E3, \u05DC\u05E0\u05EA\u05D7\
  \ \u05D0\u05D5\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח HTML כולל את חילוץ הנתונים והמידע ממסמכי HTML, מה שחשוב לצורך סריקת אינטרנט, ניתוח נתונים ומשימות אוטומטיות. מתכנתים מבצעים זאת כדי לאסוף, לנתח או לתפעל תוכן אינטרנטי תכנותית, ובכך מאפשרים את אוטומציה של מה שאחרת היה חילוץ ידני של נתונים מאתרי אינטרנט.

## איך לעשות זאת:
ל-Lua אין ספרייה מובנית לפענוח HTML, אך ניתן להשתמש בספריות של צד שלישי כמו `LuaHTML` או להיעזר בקשרים ל-`libxml2` דרך `LuaXML`. גישה פופולרית היא להשתמש בספריית `lua-gumbo` לפענוח HTML, שמספקת יכולת פענוח תקנית וישירה ל-HTML5.

### התקנת lua-gumbo:
ראשית, וודאו ש-`lua-gumbo` מותקן. בדרך כלל תוכלו להתקין אותו באמצעות luarocks:

```sh
luarocks install lua-gumbo
```

### פענוח בסיסי עם lua-gumbo:
הנה איך אפשר לפענח קטע HTML פשוט ולחלץ ממנו נתונים באמצעות `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>שלום, עולם!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Output: שלום, עולם!
```

### דוגמה מתקדמת - חילוץ קישורים:
לחילוץ מאפייני `href` מכל תגיות העוגן (`<a>` elements) במסמך HTML:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>דף לדוגמה</title></head>
<body>
  <a href="http://example.com/1">קישור 1</a>
  <a href="http://example.com/2">קישור 2</a>
  <a href="http://example.com/3">קישור 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- לוודא שזה אלמנט ויש לו מאפיינים
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- פלט לדוגמה:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

קטע הקוד הזה עובר על כל הקישורים במסמך ומדפיס את מאפייני ה-`href` שלהם. היכולת של ספריית ה-`lua-gumbo` לפענח ולהבין את מבנה מסמך HTML מפשטת את התהליך של חילוץ יסודות ספציפיים לפי התגיות או המאפיינים שלהם.
