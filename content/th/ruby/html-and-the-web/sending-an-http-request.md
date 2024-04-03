---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:05.310624-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: Ruby \u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\u0E1B\u0E47\
  \u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22 \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E40\u0E23\u0E47\u0E27\
  \u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14\u0E14\u0E49\u0E27\u0E22\u0E2B\u0E49\u0E2D\u0E07\
  \u0E2A\u0E21\u0E38\u0E14\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19 Net::HTTP."
lastmod: '2024-03-17T21:57:56.726272-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\
  \u0E33\u0E02\u0E2D HTTP \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E07\u0E48\u0E32\u0E22 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\
  \u0E17\u0E35\u0E48\u0E40\u0E23\u0E47\u0E27\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14\u0E14\
  \u0E49\u0E27\u0E22\u0E2B\u0E49\u0E2D\u0E07\u0E2A\u0E21\u0E38\u0E14\u0E21\u0E32\u0E15\
  \u0E23\u0E10\u0E32\u0E19 Net::HTTP."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีทำ:
Ruby ทำให้การส่งคำขอ HTTP เป็นเรื่องง่าย นี่คือวิธีที่เร็วที่สุดด้วยห้องสมุดมาตรฐาน Net::HTTP

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get(uri)
puts response
```

สิ่งนี้จะแสดงผลลัพธ์เนื้อหา HTML ของ `http://example.com`.

คุณอาจต้องการโพสต์ข้อมูลด้วย:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/api')
res = Net::HTTP.post_form(uri, 'key1' => 'value1', 'key2' => 'value2')
puts res.body
```

สิ่งนี้ส่งคำขอ POST พร้อมข้อมูลและแสดงคำตอบ

## ลงลึก:
ในอดีต การส่งคำขอ HTTP เป็นเรื่องยุ่งยาก และคุณอาจต้องใช้ gem เช่น `HTTParty` แต่ห้องสมุด `Net::HTTP` ที่มาพร้อมกับ Ruby นั้นได้พัฒนาไปมาก และตอนนี้รองรับสิ่งที่คุณต้องการได้ส่วนใหญ่

อย่างไรก็ตาม, `Net::HTTP` อาจดูพูดเยอะ หากโปรเจกต์ของคุณต้องการฟีเจอร์ HTTP เพิ่มเติมหรือซินแท็กซ์ที่ใช้ง่ายขึ้น, `HTTParty` หรือ `Faraday` เป็นทางเลือกที่ดี จีมเหล่านี้มี API ที่เข้าใจง่ายกว่าและสามารถรับมือกับสถานการณ์ที่ซับซ้อนมากขึ้น เช่น มิดเดิลแวร์หรืออะแดปเตอร์ที่แตกต่างกัน

โดยพื้นฐานแล้ว การส่งคำขอ HTTP ด้วย Ruby เกี่ยวข้องกับการสร้างไคลเอนต์ HTTP, ตั้งค่าวัตถุคำขอโดยมีวิธีการ, ส่วนหัว และเนื้อหาถ้าจำเป็น จากนั้นจึงส่งคำขอเพื่อรับคำตอบ

ตัวอย่างของ HTTParty:

```Ruby
require 'httparty'

response = HTTParty.get('http://example.com')
puts response.body
```

ทำเหมือน `Net::HTTP.get` แต่ด้วยการตั้งค่าน้อยลง

## ดูเพิ่มเติม:
สำหรับข้อมูลเพิ่มเติมอย่างละเอียด, คู่มือของ Ruby เป็นประโยชน์มาก:
- Net::HTTP: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty: https://github.com/jnunemaker/httparty
- Faraday: https://lostisland.github.io/faraday/

และถ้าคุณมีความสนใจอย่างหนักในการเชื่อมต่อเครือข่าย HTTP ของ Ruby, ลองดูที่:
- Ruby's Open URI: https://ruby-doc.org/stdlib/libdoc/open-uri/rdoc/OpenURI.html
- WebMock สำหรับทดสอบคำขอ HTTP: https://github.com/bblimke/webmock
