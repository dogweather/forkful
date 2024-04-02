---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:20.046856-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23 XML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\u0E04\u0E49\u0E14 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\
  \u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E48\u0E32\
  \u0E19 \u0E40\u0E02\u0E35\u0E22\u0E19 \u0E41\u0E25\u0E30\u0E41\u0E01\u0E49\u0E44\
  \u0E02\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E17\u0E35\u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u2026"
lastmod: '2024-03-17T21:57:56.378218-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\
  \u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23\
  \ XML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\u0E04\u0E49\u0E14 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E48\u0E32\u0E19 \u0E40\
  \u0E02\u0E35\u0E22\u0E19 \u0E41\u0E25\u0E30\u0E41\u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## อะไรและทำไม?
การทำงานกับ XML เกี่ยวข้องกับการแยกวิเคราะห์และจัดการเอกสาร XML โดยใช้โค้ด โปรแกรมเมอร์ทำเช่นนี้เพื่ออ่าน เขียน และแก้ไขข้อมูลในรูปแบบที่มีโครงสร้าง เคลื่อนย้ายได้อย่างหลากหลายซึ่งใช้กันอย่างแพร่หลายสำหรับการแลกเปลี่ยนและเก็บข้อมูล

## วิธีการ:
Lua ไม่รวมการแยกวิเคราะห์ XML แบบดั้งเดิม แต่มีไลบรารีเช่น LuaXML และ xml2lua ที่ทำงานนั้นได้ นี่คือการดูการแยกวิเคราะห์ XML ด้วย xml2lua อย่างรวดเร็ว:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programming in Lua</book></root>]])

print(handler.root.book._attr.id)  -- แสดงผล: 123
print(handler.root.book[1])        -- แสดงผล: Programming in Lua
```

สำหรับการเขียน XML นี่คือตัวอย่างขนาดเล็กโดยใช้ LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programming in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- แสดงผล: <root><book id="123">Programming in Lua</book></root>
```

## การศึกษาลึก
XML ย่อมาจาก Extensible Markup Language ได้เป็นมาตรฐานในการแสดงและแลกเปลี่ยนข้อมูลตั้งแต่กลางปี 90s ช่วยให้ข้อมูลมีโครงสร้างและสามารถอ่านได้ทั้งโดยมนุษย์และเครื่องจักร

ในขณะที่ JSON และ YAML ได้รับความนิยมในปัจจุบันเพราะความเรียบง่ายของพวกมัน XML ยังคงเป็นที่นิยมในหลายระบบขนาดใหญ่และระบบพื้นฐานในอดีต Lua ไม่มีการจัดการ XML แบบนั้นเพราะ Lua ถูกออกแบบให้เล็กและสามารถขยายได้ผ่านโมดูล

ไลบรารี่ XML สำหรับ Lua เช่น LuaXML, xml2lua และอื่น ๆ ช่วยเติมเต็มช่องว่างนี้ LuaXML ให้บริการอ่านและเขียน XML แบบเบา ในขณะที่ xml2lua ใช้วิธีการทำงานตามเหตุการณ์ที่คล้ายกับ SAX parsers ไลบรารีเหล่านี้มักจะถูกโค้ดใน Lua บริสุทธิ์เพื่อความพกพา ในขณะที่บางส่วนอาจพึ่งพา C เพื่อประสิทธิภาพ

เมื่อพูดถึงประสิทธิภาพและการใช้หน่วยความจำ ไลบรารี่ XML ของ Lua อาจจะไม่เร็วเท่ากับภาษาที่มีการสนับสนุนแบบดั้งเดิม อย่างไรก็ตาม สำหรับกรณีการใช้งานส่วนใหญ่ใน Lua โดยเฉพาะในการพัฒนาเกมหรือสคริปต์สำหรับระบบฝังตัว ไลบรารีเหล่านี้ทำงานได้ดีโดยไม่ทำให้ระบบล้น

## ดูเพิ่มเติม
- LuaXML บน GitHub: https://github.com/LuaDist/luaxml
- xml2lua บน GitHub: https://github.com/manoelcampos/xml2lua
- รายชื่อไลบรารี่ของ Lua.org: https://lua-users.org/wiki/LibrariesAndBindings
