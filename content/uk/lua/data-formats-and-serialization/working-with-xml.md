---
date: 2024-01-26 04:34:11.306152-07:00
description: "\u042F\u043A: Lua \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0454\
  \ \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0433\u043E \u0430\u043D\
  \u0430\u043B\u0456\u0437\u0443 XML, \u0430\u043B\u0435 \u0456\u0441\u043D\u0443\u044E\
  \u0442\u044C \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\
  \u0430\u043A\u0456 \u044F\u043A LuaXML \u0442\u0430 xml2lua, \u044F\u043A\u0456\
  \ \u0432\u0438\u043A\u043E\u043D\u0443\u044E\u0442\u044C \u0446\u044E \u0440\u043E\
  \u0431\u043E\u0442\u0443. \u041E\u0441\u044C \u0448\u0432\u0438\u0434\u043A\u0438\
  \u0439 \u043F\u043E\u0433\u043B\u044F\u0434 \u043D\u0430 \u0430\u043D\u0430\u043B\
  \u0456\u0437 XML \u0437\u2026"
lastmod: '2024-03-13T22:44:49.541239-06:00'
model: gpt-4-0125-preview
summary: "Lua \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0432\u0431\
  \u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0433\u043E \u0430\u043D\u0430\u043B\u0456\
  \u0437\u0443 XML, \u0430\u043B\u0435 \u0456\u0441\u043D\u0443\u044E\u0442\u044C\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A LuaXML \u0442\u0430 xml2lua, \u044F\u043A\u0456 \u0432\u0438\
  \u043A\u043E\u043D\u0443\u044E\u0442\u044C \u0446\u044E \u0440\u043E\u0431\u043E\
  \u0442\u0443."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як:
Lua не включає вбудованого аналізу XML, але існують бібліотеки, такі як LuaXML та xml2lua, які виконують цю роботу. Ось швидкий погляд на аналіз XML з xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Програмування на Lua</book></root>]])

print(handler.root.book._attr.id)  -- Виводить: 123
print(handler.root.book[1])        -- Виводить: Програмування на Lua
```

Для запису XML ось міні приклад із використанням LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Програмування на Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Виводить: <root><book id="123">Програмування на Lua</book></root>
```

## Поглиблене вивчення
XML, що означає Extensible Markup Language, є стандартом у представленні та обміні даними з середини 90-их. Він надає структуру даним і є одночасно зручним для читання людиною та обробки машиною.

Хоча зараз перевагу віддають JSON та YAML за їхню простоту, XML залишається поширеним у багатьох підприємницьких та спадкових системах. В Lua вбудована обробка XML не є частиною мови, оскільки Lua розроблена бути невеликою та розширюваною через модулі.

Бібліотеки XML для Lua, такі як LuaXML, xml2lua та інші, заповнюють цю прогалину. LuaXML забезпечує легкий засіб читання та запису XML, тоді як xml2lua використовує підхід на основі подій, подібний до SAX аналізаторів. Зазвичай ці бібліотеки реалізовані на чистому Lua для забезпечення портативності, хоча деякі можуть покладатися на C для підвищення продуктивності.

Що стосується продуктивності та використання пам'яті, бібліотеки XML Lua можуть не бути такими швидкими, як ті, що мають вбудовану підтримку мов. Однак для більшості випадків використання в Lua, особливо у розробці ігор або сценаріях для вбудованих систем, ці бібліотеки роблять чудову роботу без перевантаження системи.

## Дивіться також
- LuaXML на GitHub: https://github.com/LuaDist/luaxml
- xml2lua на GitHub: https://github.com/manoelcampos/xml2lua
- Список бібліотек Lua.org: https://lua-users.org/wiki/LibrariesAndBindings
