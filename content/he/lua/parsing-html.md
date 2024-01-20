---
title:                "ניתוח HTML"
date:                  2024-01-20T15:32:59.864311-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח HTML הוא תהליך שבו מתוכנת מנתח את המבנה והתוכן של דף אינטרנט. תכנותים עושים זאת כדי לאחזר, לעבד, או לשנות נתונים מדפי ווב.

## איך לעשות:
Lua אינה כוללת מודולים מובנים לפענוח HTML במובנה הקלאסי. אנו נשתמש בחבילה חיצונית כמו `luahtml` לדוגמה:

```Lua
local htmlparser = require "htmlparser"

-- טען HTML כמחרוזת
local htmlString = [[
<html>
  <head>
    <title>דוגמה</title>
  </head>
  <body>
    <h1>שלום עולם</h1>
  </body>
</html>
]]

-- פענח את ה-HTML
local root = htmlparser.parse(htmlString)

-- הדפס את כותרת הדף
local title = root:select("title")[1]
print(title:getcontent()) -- תוציא "דוגמה"
```

הקוד משתמש בפונקציה `parse` כדי להמיר את מחרוזת HTML לעץ DOM, שאותו אפשר לחפש ולחלץ ממנו מידע.

## צלילה עמוקה
במקור, Lua לא פותחה כדי לעבד את האינטרנט, ולכן אינה מספקת כלים לפענוח HTML מהקופסא. יחד עם זאת, הקהילה פיתחה ספריות כגון `luahtml` ו-`luaxml` שממלאות פער זה.

חלופות ללואה בעבודה עם HTML יכולים להיות שפות אחרות עם ספריות עשירות יותר, כמו Python עם BeautifulSoup או חבילת lxml. אבל אם רוצים להשאר עם Lua, אז השימוש בחבילות חיצוניות הוא הדרך ללכת.

מימוש פענוח HTML בלואה מתבצע בדרך כלל דרך פרסור המחרוזת והמרתה לעץ DOM, דומה לשפות אחרות. זה כולל חיפוש וחלץ אלמנטים באמצעות מתודות כמו `select` ו-`getcontent`.

## ראה גם
- למד עוד על DOM (Document Object Model): [https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)