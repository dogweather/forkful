---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:55.045962-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Lua \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19 HTTP \u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E43\u0E19\u0E15\u0E31\u0E27, \u0E14\u0E31\u0E07\u0E19\
  \u0E31\u0E49\u0E19\u0E40\u0E23\u0E32\u0E08\u0E36\u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\
  \u0E1A\u0E23\u0E32\u0E23\u0E35 \u0E40\u0E25\u0E37\u0E2D\u0E01\u0E17\u0E35\u0E48\u0E19\
  \u0E34\u0E22\u0E21\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E04\u0E37\u0E2D `lua-requests`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E46."
lastmod: '2024-03-17T21:57:56.351147-06:00'
model: gpt-4-0125-preview
summary: "Lua \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\
  \u0E2A\u0E19\u0E38\u0E19 HTTP \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E43\u0E19\u0E15\u0E31\
  \u0E27, \u0E14\u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\u0E40\u0E23\u0E32\u0E08\u0E36\
  \u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 \u0E40\u0E25\u0E37\
  \u0E2D\u0E01\u0E17\u0E35\u0E48\u0E19\u0E34\u0E22\u0E21\u0E2B\u0E19\u0E36\u0E48\u0E07\
  \u0E04\u0E37\u0E2D `lua-requests` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\
  \u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
Lua ไม่มีการสนับสนุน HTTP อย่างในตัว, ดังนั้นเราจึงใช้ไลบรารี เลือกที่นิยมหนึ่งคือ `lua-requests` นี่คือตัวอย่างง่ายๆ:

```lua
local requests = require('requests')

-- คำขอ GET
local response = requests.get('https://api.example.com/data')
print(response.status_code)
print(response.text)

-- คำขอ POST พร้อมข้อมูลบางอย่าง
local post_response = requests.post('https://api.example.com/post', {data = {key1 = 'value1', key2 = 'value2'}})
print(post_response.status_code)
print(post_response.text)
```

ตัวอย่างผลลัพธ์อาจดูเช่นนี้:

```lua
200
"{\"data\":\"นี่คือข้อมูลที่คุณขอมา!\"}"

201
"{\"success\":true,\"message\":\"ได้รับข้อมูลแล้ว!\"}"
```

## ลงลึก
ความเรียบง่ายของ Lua ไม่ครอบคลุม HTTP โดยพื้นฐาน ซึ่งเป็นที่มาของไลบรารี `lua-requests` สะท้อนฟังก์ชั่นความพร้อมใช้งานของไลบรารีคำขอ Python, ทำให้มันง่ายมากสำหรับผู้ที่คุ้นเคยกับ Python

ทางเลือกอื่น ๆ รวมถึง `LuaSocket` สำหรับงาน HTTP ระดับต่ำลง และ `luasocket.http` สำหรับการควบคุมมากขึ้น Lua ยังมีการผูกกับ `libcurl` (ผ่าน `Lua-cURL`) สำหรับการดำเนินการ HTTP ที่ซับซ้อน

ในอดีต, การขาดการสนับสนุน HTTP ภายในตัวสะท้อนถึงรากฐานของระบบฝังตัวของ Lua ที่การเขียนโปรแกรมเครือข่ายไม่ใช่เรื่องสำคัญ การพัฒนาของมันผ่านไลบรารีภายนอกสะท้อนถึงความยืดหยุ่นของชุมชนและความสามารถในการขยายของภาษา

ในทางการปฏิบัติ, เมื่อคุณส่งคำขอ HTTP, มันจะเดินทางผ่านเครือข่ายไปยังเซิร์ฟเวอร์ที่ระบุ เซิร์ฟเวอร์จะดำเนินการและตอบกลับ ไลบรารี Lua ทำให้การเขียนโปรแกรมซ็อกเก็ตที่จำเป็นง่ายขึ้น, จัดการกับทุกความซับซ้อนของการสื่อสารเครือข่ายเพื่อให้คุณสามารถโฟกัสไปที่คำขอและการตอบสนองจริง

## ดูเพิ่มเติม
- [lua-requests ที่เก็บ GitHub](https://github.com/JakobGreen/lua-requests)
- [LuaSocket คู่มืออ้างอิง](http://w3.impa.br/~diego/software/luasocket/http.html)
