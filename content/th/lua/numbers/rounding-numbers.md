---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:16.666792-06:00
description: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\u0E2B\u0E49\u0E43\u0E01\
  \u0E25\u0E49\u0E40\u0E04\u0E35\u0E22\u0E07\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\
  \u0E19\u0E40\u0E15\u0E47\u0E21\u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E28\u0E19\u0E34\u0E22\
  \u0E21\u0E17\u0E35\u0E48\u0E23\u0E30\u0E1A\u0E38\u0E44\u0E27\u0E49\u0E21\u0E32\u0E01\
  \u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14 \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\
  \u0E2B\u0E25\u0E31\u0E01\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E25\u0E14\
  \u0E04\u0E27\u0E32\u0E21\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.349257-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\u0E2B\u0E49\u0E43\u0E01\
  \u0E25\u0E49\u0E40\u0E04\u0E35\u0E22\u0E07\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\
  \u0E19\u0E40\u0E15\u0E47\u0E21\u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E28\u0E19\u0E34\u0E22\
  \u0E21\u0E17\u0E35\u0E48\u0E23\u0E30\u0E1A\u0E38\u0E44\u0E27\u0E49\u0E21\u0E32\u0E01\
  \u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14 \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\
  \u0E2B\u0E25\u0E31\u0E01\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E25\u0E14\
  \u0E04\u0E27\u0E32\u0E21\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19,\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E02\u0E2D\u0E07\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02"
weight: 13
---

## อะไร & ทำไม?
การปัดเศษตัวเลขหมายถึงการปรับตัวเลขให้ใกล้เคียงกับจำนวนเต็มหรือทศนิยมที่ระบุไว้มากที่สุด เป็นสิ่งหลักในการเขียนโปรแกรมเพื่อลดความซับซ้อน, เพิ่มประสิทธิภาพ, และสำหรับเมื่อความแม่นยำเกินจุดหนึ่งไม่ได้เพิ่มค่าใดๆ

## วิธีการ:
```lua
-- การปัดเศษตัวเลขใน Lua ไม่มีให้ตั้งแต่แรก, แต่คุณสามารถกำหนดฟังก์ชันได้:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- เพื่อปัดเศษไปยังตำแหน่งทศนิยมที่เฉพาะเจาะจง:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## ลงลึก
Lua ไม่มีฟังก์ชัน round มาให้ตั้งแต่แรกไม่เหมือนกับภาษาอื่นๆ โดยปกติแล้วคุณต้องเขียนของคุณเองหรือใช้ไลบรารีของบุคคลที่สาม วิธีแก้ปัญหาทั่วไปมักจะพึ่งพา `math.floor()` สำหรับการปัดลงและ `math.ceil()` สำหรับการปัดขึ้น, ควบคู่ไปกับการเพิ่มหรือลบ 0.5 ก่อนทำดังกล่าว, ขึ้นอยู่กับเครื่องหมายของตัวเลข

ทางเลือกในการสร้างฟังก์ชันของคุณเองรวมถึงไลบรารี เช่น "lua-users wiki" หรือ "Penlight" แต่ละอันมีข้อดีและข้อเสียของตัวเอง เช่น คุณสมบัติเพิ่มเติมหรือโหลดมากขึ้น

ภายใน, ฟังก์ชันเหล่านี้มักจะทำงานโดยใช้ประโยชน์จากวิธีที่คอมพิวเตอร์เก็บตัวเลขจุดลอยตัว การเพิ่ม 0.5 เข้าไปในจำนวนลอยตัวบวกที่คุณต้องการปัดเศษจะทำให้มันข้ามเหนือขีดความสามารถของค่าจำนวนเต็มถัดไป, ดังนั้นเมื่อคุณใช้ `math.floor()` มันจะปัดลงไปยังจำนวนเต็มที่ใกล้ที่สุด

## ดูเพิ่มเติม
- [Lua 5.4 Reference Manual: The Mathematical Functions](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua Libraries: Math](https://github.com/lunarmodules/Penlight)
