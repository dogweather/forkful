---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:36.233097-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E01\
  \u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E37\u0E49\
  \u0E19\u0E10\u0E32\u0E19\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E01\u0E32\
  \u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\
  \u0E0B\u0E34\u0E2A\u0E40\u0E15\u0E47\u0E21\u2026"
lastmod: '2024-03-17T21:57:56.368790-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E01\
  \u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E37\u0E49\
  \u0E19\u0E10\u0E32\u0E19\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E01\u0E32\
  \u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\
  \u0E0B\u0E34\u0E2A\u0E40\u0E15\u0E47\u0E21\u2026"
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่เป็นการดำเนินการพื้นฐานเมื่อเขียนสคริปต์ที่มีการโต้ตอบกับไฟล์ซิสเต็ม เพื่อให้แน่ใจว่าโปรแกรมของคุณทำงานบนเส้นทางที่ถูกต้องและป้องกันข้อผิดพลาดที่เกิดจากไดเรกทอรีที่ไม่มีอยู่จริง งานนี้มีความสำคัญต่อการสร้างไฟล์ใหม่ในไดเรกทอรี การอ่านจากไดเรกทอรี หรือการทำงานที่เฉพาะเจาะจงของไดเรกทอรีอย่างปลอดภัย

## วิธีการ:

ใน Lua, คุณไม่มีฟังก์ชันในตัวที่จะตรวจสอบโดยตรงว่ามีไดเรกทอรีอยู่หรือไม่ ดังนั้นคุณมักจะต้องพึ่งพาห้องสมุด Lua File System (lfs) ซึ่งเป็นห้องสมุดยอดนิยมสำหรับการดำเนินการกับไฟล์

ก่อนอื่นตรวจสอบให้แน่ใจว่าคุณมี Lua File System ติดตั้งแล้ว หากไม่ได้ติดตั้ง คุณสามารถติดตั้งได้โดยทั่วไปโดยใช้ LuaRocks:

```sh
luarocks install luafilesystem
```

จากนั้น คุณสามารถใช้ตัวอย่างต่อไปนี้ในการตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- ตรวจสอบว่ามีไดเรกทอรีที่ระบุอยู่หรือไม่
if directoryExists("/path/to/your/directory") then
    print("Directory exists.")
else
    print("Directory does not exist.")
end
```

ผลลัพธ์ที่ได้ออกมาเป็น:

```
Directory exists.
```

หรือ ถ้าไม่มีไดเรกทอรี:

```
Directory does not exist.
```

วิธีนี้ใช้ฟังก์ชัน `lfs.attributes` เพื่อรับคุณสมบัติของเส้นทาง หากเส้นทางมีอยู่และคุณสมบัติ `mode` ของมันคือ `directory` มันยืนยันการมีอยู่ของไดเรกทอรี
