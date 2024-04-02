---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:00.995045-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\u0E16\u0E36\u0E07\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E47\u0E2D\u0E1A\
  \u0E40\u0E08\u0E04 `Date` \u0E2B\u0E23\u0E37\u0E2D `DateTime` \u0E17\u0E35\u0E48\
  \ Ruby \u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E44\u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:56.738529-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\u0E16\u0E36\u0E07\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E47\u0E2D\u0E1A\
  \u0E40\u0E08\u0E04 `Date` \u0E2B\u0E23\u0E37\u0E2D `DateTime` \u0E17\u0E35\u0E48\
  \ Ruby \u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E44\u0E14\u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## อะไรและทำไม?
การแยกวันที่ออกจากสตริงหมายถึงการแปลงข้อความที่แสดงถึงวันที่เป็นอ็อบเจค `Date` หรือ `DateTime` ที่ Ruby เข้าใจได้ โปรแกรมเมอร์ทำเช่นนี้เพื่อทำการดำเนินการเช่นการเปรียบเทียบ การคำนวณ หรือการจัดรูปแบบวันที่ ซึ่งเป็นงานทั่วไปในแอปพลิเคชันที่มีการจัดการกับการกำหนดการ วิเคราะห์ หรือการประมวลผลข้อมูล

## วิธีการ:
ใน Ruby, ไลบรารีมาตรฐานมีวิธีการโดยตรงในการแยกวันที่จากสตริงโดยใช้คลาส `Date` และ `DateTime` นี่คือวิธีที่คุณทำได้โดยใช้เมท็อดที่มีอยู่ในตัวของ Ruby:

```ruby
require 'date'

# แยกวันที่จากสตริง
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime สำหรับการแสดงเวลาอย่างละเอียด
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

สำหรับการควบคุมมากขึ้นหรือจัดการกับรูปแบบที่ `parse` อาจไม่เข้าใจโดยตรง คุณสามารถใช้ `strptime` (การแยกสตริงตามเวลา), โดยระบุรูปแบบอย่างชัดเจน:

```ruby
# การใช้ strptime สำหรับรูปแบบที่กำหนดเอง
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### การใช้ไลบรารีภายนอก:

แม้ว่าความสามารถภายในตัวของ Ruby จะทรงพลัง แต่บางครั้งคุณอาจต้องการเลือกใช้ไลบรารีภายนอกเพื่อคุณสมบัติเพิ่มเติมหรือไวยากรณ์ที่ง่ายขึ้น หนึ่งในตัวเลือกยอดนิยมคือเจม `Chronic` สำหรับการแยกวันที่ด้วยภาษาธรรมชาติ:

1. ก่อนอื่น เพิ่ม Chronic ไปยัง Gemfile ของคุณและรัน `bundle install`:
```ruby
gem 'chronic'
```

2. จากนั้น ใช้งานดังนี้:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# ผลลัพธ์จะแตกต่างกันไปตามวันที่ปัจจุบัน; สมมติการแยกวันที่ตามวันที่ 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` มีประโยชน์มากสำหรับการป้อนข้อมูลจากผู้ใช้ เนื่องจากสามารถเข้าใจรูปแบบวันที่ในภาษาธรรมชาติได้หลากหลาย ทำให้เป็นเครื่องมือที่ทรงพลังสำหรับแอปพลิเคชันที่ต้องการการป้อนวันที่ได้อย่างยืดหยุ่น
