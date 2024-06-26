---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:08.468643-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07 Ruby\
  \ \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E04\u0E25\u0E32\u0E2A `Date` \u0E41\u0E25\
  \u0E30 `Time` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\
  \u0E27\u0E25\u0E32 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\
  \u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E43\u0E0A\u0E49\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19."
lastmod: '2024-03-17T21:57:56.739436-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19\u0E02\u0E2D\u0E07 Ruby \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E04\u0E25\
  \u0E32\u0E2A `Date` \u0E41\u0E25\u0E30 `Time` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E43\u0E0A\
  \u0E49\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\
  \u0E19."
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## วิธีการ:
ไลบรารีมาตรฐานของ Ruby รวมถึงคลาส `Date` และ `Time` สำหรับจัดการกับวันที่และเวลา นี่คือวิธีการเรียกใช้วันที่ปัจจุบัน:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

ตัวอย่างผลลัพธ์: 
```
2023-04-12
```

สำหรับการรวมเวลาไปกับวันที่ คลาส `Time` ของ Ruby เหมาะสมกว่า:

```ruby
current_time = Time.now
puts current_time
```

ตัวอย่างผลลัพธ์: 
```
2023-04-12 14:33:07 +0200
```

หากคุณต้องการฟังก์ชันการทำงานเพิ่มเติม เช่น การจัดการเขตเวลา คุณอาจต้องการใช้เจมที่สามปาร์ตี้ เช่น `ActiveSupport` (ส่วนหนึ่งของ Rails แต่สามารถใช้แยกต่างหากได้)

ขั้นแรก เพิ่ม `activesupport` ลงใน Gemfile ของคุณและรัน `bundle install`:

```ruby
gem 'activesupport'
```

จากนั้น ใช้มันเพื่อจัดการเขตเวลา:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # ตั้งค่าเขตเวลาที่คุณต้องการ
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

ตัวอย่างผลลัพธ์:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
