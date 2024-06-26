---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:28.943417-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Lua \u0E43\u0E0A\u0E49\u0E1F\
  \u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E25\u0E31\u0E01\u0E2A\u0E2D\u0E07\
  \u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\
  \u0E14\u0E1E\u0E25\u0E32\u0E14: `pcall` \u0E41\u0E25\u0E30 `xpcall` \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\
  \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19."
lastmod: '2024-03-17T21:57:56.361995-06:00'
model: gpt-4-0125-preview
summary: "Lua \u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\
  \u0E25\u0E31\u0E01\u0E2A\u0E2D\u0E07\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## วิธีการ:
Lua ใช้ฟังก์ชันหลักสองฟังก์ชันสำหรับการจัดการข้อผิดพลาด: `pcall` และ `xpcall` นี่คือวิธีที่คุณใช้งาน:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("โอ๊ะ! เกิดข้อผิดพลาดบางอย่าง.")
    else
        print("ทุกอย่างดี!")
    end
end

-- การใช้ pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("สำเร็จ!")
else
    print("จับข้อผิดพลาดได้:", errorMessage)
end

-- การใช้ xpcall พร้อมกับตัวจัดการข้อผิดพลาด
function myErrorHandler(err)
    print("ตัวจัดการข้อผิดพลาดบอกว่า:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("การเรียกนั้นสำเร็จหรือไม่?", status)
```

ผลลัพธ์ตัวอย่างอาจเป็น:

```
จับข้อผิดพลาดได้: โอ๊ะ! เกิดข้อผิดพลาดบางอย่าง.
ตัวจัดการข้อผิดพลาดบอกว่า: โอ๊ะ! เกิดข้อผิดพลาดบางอย่าง.
การเรียกนั้นสำเร็จหรือไม่? เท็จ
```
หรือถ้าไม่มีข้อผิดพลาดเกิดขึ้น:
```
ทุกอย่างดี!
สำเร็จ!
ทุกอย่างดี!
การเรียกนั้นสำเร็จหรือไม่? จริง
```

## การศึกษาเจาะลึก
การจัดการข้อผิดพลาด หรือ "การจัดการข้อยกเว้น" ไม่เคยเป็นเรื่องง่ายมาโดยตลอด โปรแกรมในช่วงแรกๆ มักจะพบกับความล้มเหลวมากมาย เมื่อการเขียนโค้ดพัฒนาขึ้น ความต้องการเรื่องความมั่นคงก็เพิ่มขึ้นเช่นกัน วิธีการของ Lua นั้นง่ายดายเมื่อเปรียบเทียบกับบางภาษา ไม่มีบล็อก `try/catch`, เพียงแค่มี `pcall` และ `xpcall` เท่านั้น pcall ป้องกันการเรียกฟังก์ชัน, ส่งกลับสถานะและข้อผิดพลาดใดๆ ในขณะที่ xpcall เพิ่มฟังก์ชันการจัดการข้อผิดพลาด, ซึ่งเหมาะสำหรับการทำความสะอาดแบบกำหนดเองหรือการบันทึก

ทางเลือกหนึ่งใน Lua คือการใช้ `assert`, ซึ่งสามารถทำหน้าที่คล้ายคลึงกันโดยการสร้างข้อผิดพลาดหากเงื่อนไขเป็นเท็จ แต่ไม่มีความยืดหยุ่นเท่ากับ `pcall` สำหรับสถานการณ์การจัดการข้อผิดพลาดที่ซับซ้อน

ในภายใน, `pcall` และ `xpcall` ทำงานโดยการตั้งค่า "สภาพแวดล้อมที่ได้รับการป้องกัน" สำหรับฟังก์ชันในการทำงาน หากมีข้อผิดพลาดปรากฎขึ้น, สภาพแวดล้อมจะจับข้อผิดพลาดนั้นและสามารถจัดการกับมันได้ทันทีหรือส่งกลับให้โปรแกรมจัดการ

## ดูเพิ่มเติม
- หนังสือการเขียนโปรแกรมด้วย Lua (ฉบับที่สาม), สามารถหาอ่านได้ที่ https://www.lua.org/pil/ สำหรับการอ่านเพิ่มเติมเกี่ยวกับการจัดการข้อผิดพลาด (ส่วนที่ 8.4)
- คู่มืออ้างอิง Lua 5.4 อย่างเป็นทางการ: https://www.lua.org/manual/5.4/ - สำหรับข้อมูลล่าสุดเกี่ยวกับฟังก์ชันการจัดการข้อผิดพลาดของ Lua
- วิกิของผู้ใช้ Lua เกี่ยวกับการจัดการข้อผิดพลาด: http://lua-users.org/wiki/ErrorHandling – สำหรับข้อมูลเชิงลึกและแบบแผนจากชุมชน
