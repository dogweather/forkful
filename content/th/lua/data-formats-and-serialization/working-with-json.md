---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:48.037674-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Lua \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \ JSON \u0E14\u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\
  \u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\
  \u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E17\u0E35\u0E48\u0E19\u0E34\u0E22\
  \u0E21\u0E04\u0E37\u0E2D `dkjson` \u0E0B\u0E36\u0E48\u0E07\u0E04\u0E38\u0E13\u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E23\u0E2B\u0E31\u0E2A\u0E41\u0E25\u0E30\
  \u0E16\u0E2D\u0E14\u0E23\u0E2B\u0E31\u0E2A JSON\u2026"
lastmod: '2024-03-17T21:57:56.375380-06:00'
model: gpt-4-0125-preview
summary: "Lua \u0E44\u0E21\u0E48\u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E43\u0E19\u0E15\u0E31\u0E27\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23 JSON \u0E14\u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\
  \u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\
  \u0E17\u0E35\u0E48\u0E19\u0E34\u0E22\u0E21\u0E04\u0E37\u0E2D `dkjson` \u0E0B\u0E36\
  \u0E48\u0E07\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E23\
  \u0E2B\u0E31\u0E2A\u0E41\u0E25\u0E30\u0E16\u0E2D\u0E14\u0E23\u0E2B\u0E31\u0E2A JSON\
  \ \u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\
  \u0E32\u0E22 \u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19 \u0E15\u0E49\u0E2D\
  \u0E07\u0E41\u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E44\u0E14\u0E49\u0E15\u0E34\
  \u0E14\u0E15\u0E31\u0E49\u0E07 `dkjson` \u0E40\u0E0A\u0E48\u0E19 \u0E1C\u0E48\u0E32\
  \u0E19 LuaRocks (`luarocks install dkjson`) \u0E41\u0E25\u0E49\u0E27\u0E17\u0E33\
  \u0E15\u0E32\u0E21\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E14\u0E49\u0E32\
  \u0E19\u0E25\u0E48\u0E32\u0E07\n\n#."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:
Lua ไม่มีไลบรารีในตัวสำหรับการจัดการ JSON ดังนั้นหนึ่งในไลบรารีของบุคคลที่สามที่นิยมคือ `dkjson` ซึ่งคุณสามารถใช้สำหรับการเข้ารหัสและถอดรหัส JSON ได้อย่างง่ายดาย ก่อนอื่น ต้องแน่ใจว่าได้ติดตั้ง `dkjson` เช่น ผ่าน LuaRocks (`luarocks install dkjson`) แล้วทำตามตัวอย่างด้านล่าง

### การถอดรหัส JSON เป็นตาราง Lua
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Error:", err)
else
  print("Name:", luaTable.name) -- ผลลัพธ์: Name: Lua Programmer
  print("Age:", luaTable.age) -- ผลลัพธ์: Age: 30
  print("Languages:", table.concat(luaTable.languages, ", ")) -- ผลลัพธ์: Languages: Lua, JavaScript
end
```

### การเข้ารหัสตาราง Lua เป็น JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua Programmer",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

ตัวอย่างผลลัพธ์สำหรับการเข้ารหัส:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua Programmer"
}
```

ตัวอย่างเหล่านี้แสดงวิธีการทำงานกับ JSON ใน Lua ทำให้การรวมแอพพลิเคชัน Lua กับเทคโนโลยีเว็บและ API ภายนอกต่าง ๆ ง่ายขึ้น จำไว้ว่า แม้ `dkjson` จะถูกใช้ในตัวอย่างเหล่านี้ แต่ไลบรารีอื่นๆ เช่น `cjson` และ `RapidJSON` ก็สามารถเป็นทางเลือกที่เหมาะสมได้เช่นกัน ขึ้นอยู่กับความต้องการของโปรเจ็คของคุณ
