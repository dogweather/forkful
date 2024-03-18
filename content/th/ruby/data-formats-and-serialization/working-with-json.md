---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:55.481157-06:00
description: "JSON (JavaScript Object Notation) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\
  \u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E21\u0E35\
  \u0E19\u0E49\u0E33\u0E2B\u0E19\u0E31\u0E01\u0E40\u0E1A\u0E32\u2026"
lastmod: '2024-03-17T21:57:56.749695-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\
  \u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E21\u0E35\u0E19\
  \u0E49\u0E33\u0E2B\u0E19\u0E31\u0E01\u0E40\u0E1A\u0E32\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

JSON (JavaScript Object Notation) เป็นรูปแบบการแลกเปลี่ยนข้อมูลที่มีน้ำหนักเบา ซึ่งแพร่หลายในเว็บแอพพลิเคชั่นสำหรับการแลกเปลี่ยนข้อมูลระหว่างไคลเอ็นต์และเซิร์ฟเวอร์ โปรแกรมเมอร์ทำงานกับ JSON ใน Ruby เพื่อแยกวิเคราะห์ข้อมูลที่ได้รับจากแหล่งภายนอกหรือเพื่อจัดรูปแบบข้อมูลสำหรับการตอบสนองของ API โดยใช้โครงสร้างที่อ่านแล้วเข้าใจง่ายเพื่อการจัดการและเก็บข้อมูลได้อย่างง่ายดาย

## การทำ:

Ruby ด้วยไลบรารีมาตรฐานของมัน มอบวิธีการที่ราบรื่นในการแยกวิเคราะห์และสร้าง JSON โมดูลหลักสำหรับการดำเนินการเหล่านี้คือ `json` ซึ่งสามารถรวมเข้ากับแอพพลิเคชั่น Ruby ได้อย่างง่ายดาย

### การแยกวิเคราะห์ JSON:

เพื่อแปลงสตริง JSON เป็นแฮชของ Ruby คุณสามารถใช้เมธอด `JSON.parse`

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Output: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### การสร้าง JSON:

ในทางกลับกัน เพื่อแปลงแฮชของ Ruby เป็นสตริง JSON คุณใช้เมธอด `JSON.generate` หรือเมธอด `to_json` ที่มีให้สำหรับอ็อบเจ็กต์ Ruby เมื่อไลบรารี `json` ถูกต้องการ

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Output: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### ไลบรารีบุคคลที่สาม:

ในขณะที่ไลบรารีมาตรฐานของ Ruby ครอบคลุมการจัดการ JSON พื้นฐาน โปรเจ็กต์มากมายพึ่งพาไลบรารีบุคคลที่สามสำหรับฟังก์ชันการทำงานและประสิทธิภาพที่เพิ่มขึ้น ตัวเลือกที่ได้รับความนิยมหนึ่งคือ `Oj` (Optimized JSON)

#### การแยกวิเคราะห์ด้วย Oj:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Output: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### การสร้างด้วย Oj:

Oj ยังมีวิธีการที่รวดเร็วในการสร้าง JSON จากอ็อบเจ็กต์ Ruby:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Output: {"name":"Samantha","age":35,"city":"Miami"}
```

ตัวอย่างเหล่านี้แสดงให้เห็นถึงความเรียบง่ายในการทำงานกับ JSON ใน Ruby ทำให้สามารถเข้าถึงได้สำหรับงานต่างๆ ตั้งแต่การจัดการข้อมูลง่ายๆ ไปจนถึงการสื่อสาร API ที่ซับซ้อน
