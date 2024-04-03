---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:10.521544-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E02\u0E31\u0E49\u0E19\
  \u0E41\u0E23\u0E01, \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E43\u0E2B\u0E49\
  \u0E41\u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\
  \u0E14\u0E25\u0E49\u0E2D\u0E21 Lua \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E21\u0E35\
  \u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E1E\u0E23\u0E40\u0E15\u0E2D\
  \u0E23\u0E4C TOML \u0E40\u0E23\u0E32\u0E08\u0E30\u0E43\u0E0A\u0E49 `lua-toml` \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E19\
  \u0E35\u0E49."
lastmod: '2024-03-17T21:57:56.377320-06:00'
model: gpt-4-0125-preview
summary: "\u0E02\u0E31\u0E49\u0E19\u0E41\u0E23\u0E01, \u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E43\u0E2B\u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E2A\
  \u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21 Lua \u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13\u0E21\u0E35\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\
  \u0E1E\u0E23\u0E40\u0E15\u0E2D\u0E23\u0E4C TOML \u0E40\u0E23\u0E32\u0E08\u0E30\u0E43\
  \u0E0A\u0E49 `lua-toml` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีการ:
ขั้นแรก, ตรวจสอบให้แน่ใจว่าสภาพแวดล้อม Lua ของคุณมีอินเทอร์เพรเตอร์ TOML เราจะใช้ `lua-toml` สำหรับตัวอย่างนี้

```Lua
local toml = require("toml")

-- แยกวิเคราะห์สตริง TOML
local toml_data = [[
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Example"

-- สร้างสตริง TOML
local table_data = {
  title = "TOML Example",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

ผลลัพธ์ตัวอย่าง:
```
TOML Example
```

## การศึกษาลงลึก
TOML ถูกสร้างขึ้นโดย Tom Preston-Werner ในปี 2013 เป็นทางเลือกอื่นที่แตกต่างจากภาษาการซีเรียลไลส์ข้อมูลอื่น ๆ อย่าง XML และ YAML โดยนำเสนอรูปแบบที่ง่ายขึ้นในการแทนค่าข้อมูลการตั้งค่า แม้ว่า JSON จะเป็นที่แพร่หลาย แต่ไวยากรณ์ของมันอาจจะยุ่งยากสำหรับไฟล์การตั้งค่า TOML โดดเด่นด้วยไวยากรณ์ที่ชัดเจนสำหรับมนุษย์ คล้ายกับไฟล์ .ini แต่มีความสามารถในการซ้อนกันและชนิดข้อมูล

ทางเลือกอื่น ๆ ที่คล้ายกับ TOML ได้แก่ JSON, YAML, และ XML อย่างไรก็ตาม TOML ถูกออกแบบมาโดยเฉพาะสำหรับการตั้งค่าและอาจถือว่าง่ายกว่า YAML, อ่านง่ายกว่า JSON สำหรับวัตถุประสงค์การตั้งค่า, และไม่เยิ่นเย้อเท่ากับ XML

การดำเนินการดูแล TOML ใน Lua โดยทั่วไปจำเป็นต้องใช้ไลบรารีของบุคคลที่สาม ประสิทธิภาพและคุณสมบัติอาจแตกต่างกันไป ตั้งแต่การวิเคราะห์พื้นฐานไปจนถึงการสนับสนุนการซีเรียลไลส์อย่างเต็มรูปแบบ เมื่อจัดการกับไฟล์การตั้งค่าขนาดใหญ่หรือการทำงานอ่าน/เขียนอย่างบ่อยครั้ง ให้พิจารณาถึงประสิทธิภาพของไลบรารีและการปฏิบัติตามเวอร์ชัน TOML ล่าสุด

## ดูเพิ่มเติม
- ข้อกำหนดของ TOML: https://toml.io/en/
- ไลบรารี `lua-toml`: https://github.com/jonstoler/lua-toml
- การเปรียบเทียบรูปแบบการซีเรียลไลส์ข้อมูล: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
