---
title:                "Робота з XML"
aliases:
- uk/lua/working-with-xml.md
date:                  2024-01-26T04:34:11.306152-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і Чому?
Робота з XML включає розбір та маніпулювання документами XML за допомогою коду. Програмісти роблять це для того, щоб читати, записувати та змінювати дані в структурованому, портативному форматі, який широко використовується для обміну даними та зберігання.

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
