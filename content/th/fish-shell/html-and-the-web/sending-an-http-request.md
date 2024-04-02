---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:02.348890-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E15\u0E32\u0E21\u0E17\u0E35\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A API\u2026"
lastmod: '2024-03-17T21:57:56.642328-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E15\u0E32\u0E21\u0E17\u0E35\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A API\u2026"
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## อะไรและทำไม?

การส่งคำขอ HTTP เป็นวิธีหนึ่งในการสื่อสารกับเว็บเซิร์ฟเวอร์ เพื่อดึงหรือส่งข้อมูลตามที่ต้องการ โปรแกรมเมอร์ใช้คำขอ HTTP เพื่อโต้ตอบกับ API หรือเว็บเซอร์วิส ซึ่งช่วยให้แอปพลิเคชันเข้าถึงแหล่งข้อมูล บริการ และข้อมูลบนอินเทอร์เน็ตได้

## วิธีทำ:

Fish ไม่มีคำสั่งในตัวสำหรับการส่งคำขอ HTTP, แต่คุณสามารถใช้ `curl` ได้โดยตรงจากเชลล์:

```Fish
curl http://api.example.com/data
```

สำหรับคำขอ POST ที่มีข้อมูล JSON:

```Fish
curl -X POST -H "Content-Type: application/json" -d '{"key":"value"}' http://api.example.com/data
```

เพื่อจัดเก็บการตอบกลับ:

```Fish
set response (curl -X GET http://api.example.com/data)
```

และนี่คือสิ่งที่คุณอาจเห็นหลังจากคำขอ GET:

```Fish
{
  "response": "Some data from the server"
}
```

## ศึกษาลึกลงไป

โดยประวัติศาสตร์แล้ว, เชลล์ใน UNIX และ Linux มีประโยชน์สำหรับงานเครือข่าย ในช่วงแรกๆ, เครื่องมือเช่น `telnet` ถูกนิยมใช้สำหรับวัตถุประสงค์ดังกล่าว ในปัจจุบัน, โปรแกรมยูทิลิตี้เช่น `curl` และ `wget` เป็นที่นิยม `curl` เป็นเครื่องมือที่มีความหลากหลาย รองรับโปรโตคอลหลายรูปแบบ และมักถูกใช้เพราะความง่ายและความยืดหยุ่น

Python หรือ Node.js สามารถใช้เมื่อคุณต้องการจัดการคำขอที่ซับซ้อนมากขึ้น แต่สำหรับงานเล็กๆ หรือสคริปต์ง่ายๆ, `curl` ใน Fish เป็นทางเลือกที่มีประสิทธิภาพและได้ผลดี

การนำแนวคิด HTTP request มาใช้ผ่าน Fish หมายความว่ามักต้องพึ่งพาเครื่องมือของบุคคลที่สาม Fish เองถูกออกแบบมาเพื่อเป็นเชลล์บรรทัดคำสั่งที่ฉลาดและเป็นมิตรต่อผู้ใช้ ไม่ใช่เครื่องมือที่ทำทุกอย่างได้ เมื่อคุณรวมมันกับพลังของยูทิลิตี้เช่น `curl`, คุณได้รับสิ่งที่ดีที่สุดจากทั้งสองโลก: ความสามารถใช้งานของ Fish และความสามารถของ `curl`.

## ดูข้อมูลเพิ่มเติม

- เรียนรู้เพิ่มเติมเกี่ยวกับ `curl`: https://curl.se/docs/manual.html
- เอกสาร Fish Shell: https://fishshell.com/docs/current/index.html
- ภาพรวมพื้นฐานของ HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- สำรวจ API ด้วย `httpie`, ทางเลือกหนึ่งสำหรับ `curl`: https://httpie.io/
