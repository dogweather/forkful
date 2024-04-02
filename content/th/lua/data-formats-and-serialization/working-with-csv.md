---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:51.205649-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\
  \u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E08\u0E31\
  \u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\u0E40\u0E1B\u0E47\u0E19\u0E41\u0E16\
  \u0E27\u0E41\u0E25\u0E30\u0E04\u0E2D\u0E25\u0E31\u0E21\u0E19\u0E4C \u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E08\u0E38\u0E25\u0E20\u0E32\u0E04\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E41\u0E22\u0E01\u0E04\u0E48\u0E32\u0E41\u0E15\u0E48\u0E25\u0E30\u0E23\u0E32\u0E22\
  \u0E01\u0E32\u0E23\u2026"
lastmod: '2024-03-17T21:57:56.376347-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\
  \u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E08\u0E31\
  \u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\u0E40\u0E1B\u0E47\u0E19\u0E41\u0E16\
  \u0E27\u0E41\u0E25\u0E30\u0E04\u0E2D\u0E25\u0E31\u0E21\u0E19\u0E4C \u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E08\u0E38\u0E25\u0E20\u0E32\u0E04\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E41\u0E22\u0E01\u0E04\u0E48\u0E32\u0E41\u0E15\u0E48\u0E25\u0E30\u0E23\u0E32\u0E22\
  \u0E01\u0E32\u0E23\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## อะไร & ทำไม?

การทำงานกับไฟล์ CSV (Comma-Separated Values) หมายถึงการแยกวิเคราะห์และสร้างข้อมูลข้อความที่จัดระเบียบเป็นแถวและคอลัมน์ โดยใช้จุลภาคเพื่อแยกค่าแต่ละรายการ โปรแกรมเมอร์มักจะทำกระบวนการนี้เพื่อส่งเสริมการแลกเปลี่ยนข้อมูลระหว่างแอปพลิเคชั่นที่แตกต่างกัน ฐานข้อมูล หรือสำหรับงานด้านการประมวลผลและวิเคราะห์ข้อมูล เนื่องจาก CSV ได้รับการสนับสนุนอย่างกว้างขวางและความเรียบง่าย

## วิธีการ:

ใน Lua, การทำงานกับไฟล์ CSV สามารถทำได้โดยใช้การดำเนินการ IO ของไฟล์พื้นฐานที่ภาษานี้ให้มา ไม่ต้องการไลบรารีภายนอกสำหรับงานเรียบง่าย สำหรับการดำเนินการที่ซับซ้อนกว่า เช่น การจัดการกรณีพิเศษ (เช่น จุลภาคภายในค่า) อาจเป็นประโยชน์ที่จะใช้ไลบรารีของบุคคลที่สาม เช่น `lua-csv`

### การอ่านไฟล์ CSV
นี่คือตัวอย่างง่าย ๆ ในการอ่านไฟล์ CSV ทีละบรรทัด โดยแบ่งแต่ละบรรทัดออกเป็นค่าตามตัวคั่นจุลภาค

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**ตัวอย่างผลลัพธ์** (สำหรับ `example.csv` ที่มีเนื้อหา "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### การเขียนไฟล์ CSV
เพื่อสร้างไฟล์ CSV คุณเพียงแต่สร้างสตริงที่มีค่าแยกด้วยจุลภาคและเขียนลงในไฟล์ทีละบรรทัด

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

สิ่งนี้จะสร้าง (หรือเขียนทับ) ไฟล์ `output.csv` ด้วยข้อมูลที่ระบุ

### การใช้ lua-csv
สำหรับการจัดการ CSV ที่ซับซ้อนกว่า ซึ่งรวมถึงการสนับสนุนคำพูดและตัวละเว้น ไลบรารี `lua-csv` เป็นทางเลือกที่มีความเข้มแข็ง

ก่อนอื่น ติดตั้งโดยใช้ LuaRocks:
```shell
luarocks install lua-csv
```

จากนั้น การอ่านไฟล์ CSV กลายเป็นเรื่องง่าย:

```lua
local csv = require("csv")

-- การอ่านจากไฟล์
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

และการเขียนไปยัง CSV ด้วยการอ้างถึงและหลีกเลี่ยงอย่างถูกต้อง:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

วิธีนี้จัดการโดยอัตโนมัติกับความซับซ้อน เช่น จุลภาคและคำพูดภายในค่า
