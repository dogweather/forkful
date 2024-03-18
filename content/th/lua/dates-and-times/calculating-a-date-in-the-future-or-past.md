---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:16.967699-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E41\u0E1B\u0E25\u0E27\u0E48\u0E32\u0E01\u0E33\u0E25\u0E31\u0E07\
  \u0E2B\u0E32\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E30\u0E40\
  \u0E1B\u0E47\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E2B\u0E25\u0E31\u0E07\
  \u0E08\u0E32\u0E01\u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E48\u0E2D\u0E19\u0E0A\u0E48\u0E27\
  \u0E07\u0E40\u0E27\u0E25\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E46 \u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E04\u0E38\u0E13\u0E2A\u0E21\u0E1A\u0E31\u0E15\
  \u0E34\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E41\u0E08\u0E49\u0E07\u0E40\u0E15\
  \u0E37\u0E2D\u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.367853-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E41\u0E1B\u0E25\u0E27\u0E48\u0E32\u0E01\u0E33\u0E25\u0E31\u0E07\
  \u0E2B\u0E32\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E30\u0E40\
  \u0E1B\u0E47\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E2B\u0E25\u0E31\u0E07\
  \u0E08\u0E32\u0E01\u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E48\u0E2D\u0E19\u0E0A\u0E48\u0E27\
  \u0E07\u0E40\u0E27\u0E25\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E46 \u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E04\u0E38\u0E13\u0E2A\u0E21\u0E1A\u0E31\u0E15\
  \u0E34\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E41\u0E08\u0E49\u0E07\u0E40\u0E15\
  \u0E37\u0E2D\u0E19,\u2026"
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การคำนวณวันที่ในอนาคตหรืออดีตแปลว่ากำลังหาว่าวันที่จะเป็นอย่างไรหลังจากหรือก่อนช่วงเวลาหนึ่งๆ นักพัฒนาทำเช่นนี้สำหรับคุณสมบัติเช่นการแจ้งเตือน, การสมัครสมาชิก, หรือติดตามเหตุการณ์ในอดีต

## วิธีการ:

ใน Lua, คุณมีฟังก์ชัน `os.date` และ `os.time` ที่คุณใช้เพื่อช่วยในการคำนวณวันที่และเวลา

```Lua
-- เพิ่มวันไปยังวันที่ปัจจุบัน
local daysToAdd = 10
local futureDate = os.time() + (daysToAdd * 24 * 60 * 60) -- วัน * ชั่วโมง * นาที * วินาที
print("Future Date: " .. os.date("%Y-%m-%d", futureDate))

-- ลบวันจากวันที่ปัจจุบัน
local daysToSubtract = 5
local pastDate = os.time() - (daysToSubtract * 24 * 60 * 60) -- การแปลงเดียวกันเหมือนข้างบน
print("Past Date: " .. os.date("%Y-%m-%d", pastDate))
```

ผลลัพธ์ตัวอย่างอาจเป็น:
```
Future Date: 2023-05-03
Past Date: 2023-04-18
```

## การลงลึก

ฟังก์ชัน `os.date` และ `os.time` ของ Lua มีรากฐานมาจาก C library มาตรฐาน นี่หมายความว่าพวกเขาใกล้เคียงกับเมทัล — มีประสิทธิภาพและเชื่อถือได้ พวกเขาไม่ใช้ของตกแต่งเช่นเขตเวลาหรือการปรับเวลาตามฤดูกาล; พวกเขาจัดการใน UTC และวินาทีตั้งแต่ Unix epoch (1 มกราคม 1970)

หากคุณกำลังมองหามากกว่า `os.date` และ `os.time` มีทางเลือกอื่นที่มีอยู่ ไลบรารีเช่น `Luadate` นำเสนอการดำเนินการที่ซับซ้อนมากขึ้น, การจัดการเขตเวลาและปรับเวลาตามฤดูกาลด้วยความประณีตมากขึ้น

เมื่อพูดถึงการนำไปใช้, จับตาดูวินาทีที่เพิ่มขึ้น, และจำไว้ว่าการเพิ่มเดือนไม่เรียบง่ายเหมือนกับการเพิ่ม 30 วัน แต่ละเดือนมีจำนวนวันที่แตกต่างกัน, และกุมภาพันธ์อาจทำให้คุณเสียเวลาหรือประหลาดใจด้วยวันเพิ่มเติม

## ดูเพิ่มเติม

สำหรับประสบการณ์วันและเวลาที่หรูหรายิ่งขึ้นใน Lua, ตรวจสอบทรัพยากรเหล่านี้:

- LuaRocks `Luadate`: https://luarocks.org/modules/luarocks/luadate
- วิกิผู้ใช้ Lua เกี่ยวกับวันที่และเวลา: http://lua-users.org/wiki/DateTime
- อ้างอิงไลบรารี `os` ในคู่มือ Lua 5.4: https://www.lua.org/manual/5.4/manual.html#6.9
