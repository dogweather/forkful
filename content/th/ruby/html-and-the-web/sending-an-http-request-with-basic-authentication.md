---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:45.905450-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E42\u0E14\u0E22\u0E21\u0E35\
  \u0E01\u0E32\u0E23\u0E22\u0E37\u0E19\u0E22\u0E31\u0E19\u0E15\u0E31\u0E27\u0E15\u0E19\
  \u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19 \u0E04\u0E38\u0E13\u0E08\
  \u0E30\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `Net::HTTP` \u0E43\u0E19\
  \ Ruby \u0E42\u0E14\u0E22\u0E1B\u0E01\u0E15\u0E34 \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
lastmod: '2024-03-17T21:57:56.729100-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D\
  \ HTTP \u0E42\u0E14\u0E22\u0E21\u0E35\u0E01\u0E32\u0E23\u0E22\u0E37\u0E19\u0E22\u0E31\
  \u0E19\u0E15\u0E31\u0E27\u0E15\u0E19\u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\
  \u0E32\u0E19 \u0E04\u0E38\u0E13\u0E08\u0E30\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\
  \u0E25 `Net::HTTP` \u0E43\u0E19 Ruby \u0E42\u0E14\u0E22\u0E1B\u0E01\u0E15\u0E34\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
เพื่อส่งคำขอ HTTP โดยมีการยืนยันตัวตนแบบพื้นฐาน คุณจะใช้โมดูล `Net::HTTP` ใน Ruby โดยปกติ นี่คือตัวอย่างอย่างรวดเร็ว:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
username = 'your_username'
password = 'your_password'

request = Net::HTTP::Get.new(uri)
request.basic_auth(username, password)

response = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(request)
}

puts response.body
```

ถ้าคุณรันโค้ดนี้ด้วยข้อมูลประจำตัวที่ถูกต้อง คุณจะเห็นเนื้อหาตอบกลับแสดงออกมา ถ้าข้อมูลประจำตัวไม่ถูกต้อง คุณจะได้รับข้อความแสดงข้อผิดพลาด

## ศึกษาเพิ่มเติม
การยืนยันตัวตนแบบพื้นฐานมีประวัติยาวนานเป็นส่วนหนึ่งของโพรโทคอลเว็บ ย้อนกลับไปถึง RFC แรกๆที่กำหนดการทำงานของอินเทอร์เน็ต มันเป็นวิธีง่ายๆในการควบคุมการเข้าถึง: ชื่อผู้ใช้และรหัสผ่านถูกเข้ารหัสด้วย Base64 และส่งผ่านส่วนหัว HTTP

อย่างไรก็ตาม การยืนยันตัวตนแบบพื้นฐานส่งข้อมูลประจำตัวในรูปแบบข้อความธรรมดา (ถึงแม้จะเข้ารหัสแล้วก็ตาม) ดังนั้นมันจึงไม่ปลอดภัยเมื่อใช้งานผ่าน HTTP จึงดีกว่าที่จะใช้ HTTPS เพื่อรักษาความปลอดภัยของข้อมูลประจำตัวจากการถูกมองเห็นโดยผู้อื่น

มีทางเลือกที่ปลอดภัยกว่าเช่น OAuth ซึ่งมักใช้สำหรับการยืนยันตัวตนของ API OAuth อนุญาตให้ผู้ใช้เข้าพักสิทธิ์การเข้าถึงของบุคคลที่สามโดยไม่ใช้ข้อมูลประจำตัว อย่างไรก็ตาม การยืนยันตัวตนแบบพื้นฐานยังคงใช้อยู่ โดยเฉพาะอย่างยิ่งสำหรับแอปพลิเคชันภายในและสคริปต์ที่ต้องการการทำงานอย่างรวดเร็วและง่ายดาย

รายละเอียดหนึ่งที่ควรทราบคือ `Net::HTTP` ของ Ruby จะไม่จัดการ Basic Auth โดยตรงจนกว่าคุณจะใช้วิธี `basic_auth` อย่างชัดเจน นอกจากนี้ยังสำคัญที่ต้องจัดการกับข้อยกเว้นและคำตอบผิดพลาดที่อาจเกิดขึ้นจากคำขอ HTTP ได้อย่างถูกต้อง

## ดูเพิ่มเติม
- เอกสาร `Net::HTTP` ของ Ruby standard library: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- บทนำเกี่ยวกับ OAuth สำหรับการยืนยันตัวตน: https://oauth.net/2/
- เพิ่มเติมเกี่ยวกับ Ruby และคำขอ HTTP: https://www.rubyguides.com/2019/08/ruby-http-request/
