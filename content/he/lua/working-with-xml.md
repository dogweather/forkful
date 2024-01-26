---
title:                "עבודה עם XML"
date:                  2024-01-26T04:34:05.209738-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-xml.md"
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