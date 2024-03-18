---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:14.302902-06:00
description: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E19\u0E31\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E02\
  \u0E2D\u0E07\u0E21\u0E31\u0E19 \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E41\u0E15\u0E48\
  \u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\
  \u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E04\u0E27\u0E32\u0E21\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07, \u0E01\u0E32\
  \u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.718624-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E19\u0E31\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E02\
  \u0E2D\u0E07\u0E21\u0E31\u0E19 \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E41\u0E15\u0E48\
  \u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\
  \u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E04\u0E27\u0E32\u0E21\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07, \u0E01\u0E32\
  \u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21,\u2026"
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การหาความยาวของสตริงหมายถึงการนับอักขระของมัน มันเป็นเรื่องพื้นฐานแต่สำคัญสำหรับงานเช่นการตรวจสอบความถูกต้อง, การประมวลผลข้อความ, และการกำหนดความต้องการเก็บข้อมูล

## วิธีการ:
Ruby ทำให้มันเรียบง่ายด้วยเมธอด `.length`:

```ruby
greeting = "Hello, world!"
puts greeting.length
```

ผลลัพธ์:

```
13
```

หรือใช้ `.size` ซึ่งทำงานเหมือนกัน:

```ruby
greeting = "Hello, world!"
puts greeting.size
```

ผลลัพธ์:

```
13
```

## ดำดิ่งลึก
ใน Ruby, `.length` และ `.size` สามารถใช้แทนกันได้เมื่อพูดถึงสตริง; พวกเขาให้คุณนับจำนวนอักขระ Ruby โดยประวัติศาสตร์ได้มุ่งเน้นทำให้โค้ดอ่านง่ายขึ้น, นั่นเป็นเหตุผลที่คุณมักจะพบว่ามีมากกว่าหนึ่งวิธีในการทำสิ่งเดียวกัน

ภายในนั้น, แต่ละอักขระในสตริงมีผลต่อขนาดการเก็บข้อมูล ดังนั้นการทราบจำนวนสามารถสำคัญสำหรับการเพิ่มประสิทธิภาพ โดยเฉพาะกับปริมาณข้อความมหาศาล

ในขณะที่ `.length` และ `.size` ให้คุณนับจำนวนอักขระ ในบางภาษาและในอดีต ความยาวของสตริงอาจหมายถึงขนาดไบต์ อย่างไรก็ตาม Ruby ที่มีการสนับสนุนอักขระหลายไบต์ผ่าน Unicode ไม่ได้เทียบขนาดไบต์โดยตรงกับความยาวสตริงเนื่องจากอักขระอาจใช้ไบต์มากกว่าหนึ่ง

ตัวเลือกอื่นเช่น `.bytesize` บอกคุณว่าสตริงใช้ไบต์มากแค่ไหน และ `.chars.count` ให้คุณจำนวนอักขระโดยการแปลงสตริงเป็นอาร์เรย์ของอักขระก่อน

นี่คือวิธีที่คุณสามารถใช้ `.bytesize` และ `.chars.count`:

```ruby
greeting = "Hello, world!"
puts greeting.bytesize
puts greeting.chars.count
```

ผลลัพธ์:

```
13
13
```

## ดูเพิ่มเติม
- เอกสารประกอบ Ruby เกี่ยวกับ Strings: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- บทความบทนำที่ดีเรื่อง Ruby Strings โดย [RubyGuides](https://www.rubyguides.com/2018/01/ruby-string-methods/): สำรวจเพิ่มเติมเกี่ยวกับสิ่งที่คุณสามารถทำกับสตริงนอกเหนือจากการวัดขนาดของพวกมัน
- ตกลงใจสำรวจการเข้ารหัสอักขระและวิธีการที่มีผลต่อการดำเนินการสตริงด้วย [บทความนี้จาก Thoughtbot](https://thoughtbot.com/blog/its-about-time-zones#character-encoding).
