---
date: 2024-01-26 04:34:05.209738-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E8\u05E1\u05D5\u05E8 \u05D5\u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\
  \u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05E1\u05DE\u05DB\u05D9 XML \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA \u05E7\u05D5\u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA\
  \ \u05DC\u05E7\u05E8\u05D5\u05D0, \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D5\u05DC\u05E9\
  \u05E0\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D5\u05E0\u05D9\u05D9\u05D3 \u05E9\
  \u05DE\u05E9\u05DE\u05E9 \u05DC\u05E8\u05D5\u05D1 \u05DC\u05D4\u05D7\u05DC\u05E4\
  \u05EA\u2026"
lastmod: '2024-03-11T00:14:13.071617-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E8\u05E1\u05D5\u05E8 \u05D5\u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\
  \u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05E1\u05DE\u05DB\u05D9 XML \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA \u05E7\u05D5\u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA\
  \ \u05DC\u05E7\u05E8\u05D5\u05D0, \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D5\u05DC\u05E9\
  \u05E0\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D5\u05E0\u05D9\u05D9\u05D3 \u05E9\
  \u05DE\u05E9\u05DE\u05E9 \u05DC\u05E8\u05D5\u05D1 \u05DC\u05D4\u05D7\u05DC\u05E4\
  \u05EA\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת פרסור ומניפולציה של מסמכי XML באמצעות קוד. תכנתים עושים זאת על מנת לקרוא, לכתוב ולשנות נתונים בפורמט מובנה ונייד שמשמש לרוב להחלפת נתונים ואחסון.

## איך לעשות:
Lua אינו כולל פרסור XML מובנה, אך ישנן ספריות כמו LuaXML ו־xml2lua שמבצעות את העבודה. הנה מבט מהיר על פרסור XML באמצעות xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">תכנות ב-Lua</book></root>]])

print(handler.root.book._attr.id)  -- מוציא: 123
print(handler.root.book[1])        -- מוציא: תכנות ב-Lua
```

לכתיבה של XML, הנה דוגמה מינימלית באמצעות LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "תכנות ב-Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- מוציא: <root><book id="123">תכנות ב-Lua</book></root>
```

## צלילה עמוקה
XML, שמתרגם לשפת הסימון הנתיחה (Extensible Markup Language), הוא תקן בייצוג נתונים והחלפה מאמצע שנות ה-90. הוא נותן מבנה לנתונים והוא קריא גם לאנשים וגם למחשבים.

בעוד ש-JSON ו-YAML מועדפים כעת בגלל הפשטות שלהם, XML עדיין שורר במערכות רבות ארגוניות ומורשות. ב-Lua, טיפול ילידי ב-XML אינו מוטמע מכיוון ש-Lua מעוצבת להיות קטנה ונתיחה דרך מודולים.

ספריות XML ל-Lua, כמו LuaXML, xml2lua ואחרים, גורמות לגשר על הפער הזה. LuaXML מספקת קורא וכותב XML קליל, בעוד ש-xml2lua משתמשת בגישה מבוססת אירועים דומה למפענחי SAX. ספריות אלו בדרך כלל מיושמות ב-Lua נקי לצורך ניידות, וחלק מהן עשויות להסתמך על C לצורך ביצועים.

בנוגע לביצועים ושימוש בזיכרון, ספריות XML של Lua עשויות לא להיות מהירות כמו אלו בשפות עם תמיכה ילידית. עם זאת, לרוב השימושים ב-Lua, במיוחד בפיתוח משחקים או כתיבת סקריפטים למערכות מוטמעות, ספריות אלו עושות עבודה טובה ללא עומס יתר על המערכת.

## ראו גם
- LuaXML ב-GitHub: https://github.com/LuaDist/luaxml
- xml2lua ב-GitHub: https://github.com/manoelcampos/xml2lua
- רשימת ספריות של Lua.org: https://lua-users.org/wiki/LibrariesAndBindings
