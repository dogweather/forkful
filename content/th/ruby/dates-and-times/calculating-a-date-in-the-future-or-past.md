---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:53.295807-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Ruby \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E01\u0E32\u0E23\u0E40\u0E25\u0E48\u0E19\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22\u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `Date` \u0E43\u0E19\u0E15\u0E31\u0E27\u0E41\
  \u0E25\u0E30 gem `active_support` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E1E\u0E34\
  \u0E48\u0E21\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\u0E14\u0E27\u0E01\u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 \u0E40\u0E23\u0E32\u0E21\u0E32\u0E14\
  \u0E39\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33\u0E01\u0E31\u0E19."
lastmod: '2024-03-17T21:57:56.742247-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E25\u0E48\u0E19\
  \u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\u0E14\
  \u0E32\u0E22\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `Date`\
  \ \u0E43\u0E19\u0E15\u0E31\u0E27\u0E41\u0E25\u0E30 gem `active_support` \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\
  \u0E14\u0E27\u0E01\u0E43\u0E19\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\
  \ \u0E40\u0E23\u0E32\u0E21\u0E32\u0E14\u0E39\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33\
  \u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## วิธีการ:
Ruby ทำให้การเล่นกับวันที่ง่ายดายโดยใช้คลาส `Date` ในตัวและ gem `active_support` เพื่อเพิ่มความสะดวกในการใช้งาน เรามาดูวิธีทำกัน:

```Ruby
require 'date'
require 'active_support/core_ext/integer'

# รับวันที่ของวันนี้
today = Date.today
puts "วันนี้คือ: #{today}"

# คำนวณวันที่ในอนาคต 10 วัน
future_date = today + 10
puts "10 วันข้างหน้าจะเป็น: #{future_date}"

# คำนวณวันที่ในอดีต 30 วัน
past_date = today - 30
puts "30 วันที่แล้วเป็น: #{past_date}"

# การคำนวณที่ซับซ้อนมากขึ้นกับ active_support
puts "ใน 2 เดือนข้างหน้าจะเป็น: #{2.months.from_now.to_date}"
puts "100 วันที่แล้วมันเป็น: #{100.days.ago.to_date}"
```

ผลลัพธ์ที่ตัวอย่าง:

```
วันนี้คือ: 2023-04-07
10 วันข้างหน้าจะเป็น: 2023-04-17
30 วันที่แล้วเป็น: 2023-03-08
ใน 2 เดือนข้างหน้าจะเป็น: 2023-06-07
100 วันที่แล้วมันเป็น: 2022-12-28
```

## รายละเอียดเพิ่มเติม
ก่อนที่ Ruby จะรวมฟังก์ชันการคำนวณวันที่เข้ากับไลบรารีมาตรฐานและเพิ่มเติม, นักพัฒนามักต้องคำนวณวันที่ด้วยตัวเอง โดยพิจารณาถึงปีอธิกสุรทิน, ความยาวของเดือนที่ต่างกัน, และโซนเวลา—ใช่, เป็นเรื่องที่ปวดหัว

คลาส `Date` มาตรฐานทำสิ่งต่าง ๆ ได้มากโดยตรง คุณสามารถเพิ่ม (`+`) หรือหัก (`-`) วันได้อย่างง่ายดาย อย่างไรก็ตาม, สำหรับการจัดการช่วงเวลาที่มีความหมายมากขึ้นเช่น "2 เดือนจากนี้", เราต้องพึ่งพา `active_support`, ซึ่งแยกออกมาจาก Ruby on Rails บรรจุภัณฑ์นี้ใช้การขยายคลาส Ruby มาตรฐาน, ทำให้การคำนวณเหล่านี้เป็นมิตรกับผู้ใช้

เมื่อคำนวณวันที่ในอดีตหรืออนาคต, ถ้าคุณยังคำนวณเวลาด้วย (`DateTime` หรือ `Time` อ็อบเจกต์) คุณควรพิจารณาถึงโซนเวลาด้วย คลาส `Time` ของ Ruby และ `active_support` สามารถจัดการกับสิ่งนี้ได้ แต่ต้องการการตั้งค่าเพิ่มเติม

มีตัวเลือกอื่น ๆ เช่น บรรจุภัณฑ์ `time-lord` และ `ice_cube`, ที่นำเสนอการหวานปากหรือคุณสมบัติเฉพาะ (เช่น เหตุการณ์ที่เกิดซ้ำ), ตามลำดับ

## ดูเพิ่มเติมที่
- การจัดการกับโซนเวลาใน Ruby: [https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html)
- บรรจุภัณฑ์ 'time-lord' สำหรับการแสดงออกที่ใกล้เคียงกับมนุษย์มากขึ้น: [https://github.com/krainboltgreene/time-lord](https://github.com/krainboltgreene/time-lord)
- บรรจุภัณฑ์ 'ice_cube' สำหรับการจัดการกับเหตุการณ์ที่เกิดขึ้นซ้ำ: [https://github.com/seejohnrun/ice_cube](https://github.com/seejohnrun/ice_cube)
