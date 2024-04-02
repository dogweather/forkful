---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:41.145670-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E25\u0E30\
  \u0E02\u0E48\u0E32\u0E27\u0E2A\u0E32\u0E23\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23 HTML \u0E0B\u0E36\u0E48\u0E07\u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2A\
  \u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\
  \u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E31\u0E1A (web scraping), \u0E01\u0E32\u0E23\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25 \u0E41\u0E25\u0E30\u0E07\u0E32\u0E19\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\
  \u0E15\u0E34\u2026"
lastmod: '2024-03-17T21:57:56.352086-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E25\u0E30\
  \u0E02\u0E48\u0E32\u0E27\u0E2A\u0E32\u0E23\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23 HTML \u0E0B\u0E36\u0E48\u0E07\u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2A\
  \u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\
  \u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E31\u0E1A (web scraping), \u0E01\u0E32\u0E23\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25 \u0E41\u0E25\u0E30\u0E07\u0E32\u0E19\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\
  \u0E15\u0E34\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## อะไรและทำไม?
การแยกส่วน HTML เกี่ยวข้องกับการสกัดข้อมูลและข่าวสารจากเอกสาร HTML ซึ่งมีความสำคัญสำหรับการเว็บสกรับ (web scraping), การวิเคราะห์ข้อมูล และงานอัตโนมัติ โปรแกรมเมอร์ทำเช่นนี้เพื่อรวบรวม, วิเคราะห์ หรือจัดการเนื้อหาเว็บโดยโปรแกรม เพื่อเปิดใช้งานการอัตโนมัติของการสกัดข้อมูลจากเว็บไซต์ซึ่งมิเช่นนั้นอาจต้องทำด้วยวิธีการแบบแมนนวล

## วิธีการ:
Lua ไม่มีไลบรารีที่ใช้สำหรับการแยกส่วน HTML โดยตรง แต่คุณสามารถใช้ไลบรารีของบุคคลที่สาม เช่น `LuaHTML` หรือใช้พันธุ์ (bindings) สำหรับ `libxml2` ผ่าน `LuaXML` วิธีการที่นิยมคือการใช้ไลบรารี `lua-gumbo` เพื่อการแยกส่วน HTML ซึ่งมอบความสามารถในการแยกส่วนที่สอดคล้องกับ HTML5 อย่างง่ายดาย

### การติดตั้ง lua-gumbo:
ขั้นแรก ตรวจสอบว่า `lua-gumbo` ได้ถูกติดตั้ง คุณสามารถติดตั้งได้โดยปกติผ่าน luarocks:

```sh
luarocks install lua-gumbo
```

### การแยกส่วนพื้นฐานกับ lua-gumbo:
นี่คือวิธีการแยกส่วนชิ้นส่วน HTML ง่ายๆ และสกัดข้อมูลจากมันโดยใช้ `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>สวัสดี, โลก!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- ผลลัพธ์: สวัสดี, โลก!
```

### ตัวอย่างขั้นสูง - การสกัดลิงก์:
เพื่อสกัดคุณลักษณะ `href` จากแท็กของทุก anchor (`<a>` elements) ในเอกสาร HTML:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>หน้าตัวอย่าง</title></head>
<body>
  <a href="http://example.com/1">ลิงก์ 1</a>
  <a href="http://example.com/2">ลิงก์ 2</a>
  <a href="http://example.com/3">ลิงก์ 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- ตรวจสอบว่าเป็น Element และมี attributes
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- ผลลัพธ์ตัวอย่าง:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

ชิ้นส่วนของโค้ดนี้จะเดินทางผ่านลิงก์ทั้งหมดในเอกสารและพิมพ์คุณลักษณะ `href` ของพวกมัน ความสามารถของไลบรารี `lua-gumbo` ในการแยกส่วนและเข้าใจโครงสร้างของเอกสาร HTML ทำให้กระบวนการของการสกัดองค์ประกอบเฉพาะตามแท็กหรือคุณลักษณะของพวกมันง่ายขึ้น
