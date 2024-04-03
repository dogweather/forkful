---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:19.700953-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22 \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E08\u0E32\u0E01\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E25\u0E31\u0E01 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E41\u0E25\u0E30\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07 \u2014 \u0E40\
  \u0E0A\u0E48\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.716794-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22 \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E08\u0E32\u0E01\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E25\u0E31\u0E01 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E41\u0E25\u0E30\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07 \u2014 \u0E40\
  \u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E2B\u0E32\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E08\u0E32\u0E01\u0E17\u0E35\u0E48\u0E2D\
  \u0E22\u0E39\u0E48\u0E2D\u0E35\u0E40\u0E21\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\
  \u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E40\
  \u0E27\u0E25\u0E32\u0E2D\u0E2D\u0E01\u0E21\u0E32."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## วิธีการ:
Ruby ทำให้การดึงข้อความย่อยเป็นเรื่องง่าย มาดูวิธีการกันเลย:

```Ruby
str = "Hello, Ruby World!"

# วิธีที่ 1: ใช้ดัชนีอาร์เรย์
substring = str[7, 4] # "Ruby"
puts substring

# วิธีที่ 2: ใช้เมธอด slice
slice = str.slice(7, 4) # "Ruby"
puts slice

# วิธีที่ 3: นิพจน์ประจำกาล
match = str[/[Rr]uby/] # "Ruby"
puts match

# วิธีที่ 4: split และการเข้าถึงอาร์เรย์
split_array = str.split # ค่าเริ่มต้นจะแบ่งโดยช่องว่าง
picked_word = split_array[2] # "World!"
puts picked_word
```

ผลลัพธ์ที่แสดงจากตัวอย่างแต่ละส่วนจะเป็น "Ruby", "Ruby", "Ruby", "World!" ตามลำดับ

## ลงลึก
ในอดีต, การดึงข้อความย่อยเป็นกระบวนการที่ยาวนานกว่านี้ Ruby ได้พัฒนาขึ้น ณ วันนี้ คุณมีเมธอดและ regex ที่สามารถใช้งานได้

นี่คือสิ่งที่เกิดขึ้นภายใต้การทำงาน: `[7, 4]` หมายถึง เริ่มที่อักขระที่ 7 และดึงข้อมูล 4 ตัวถัดไป `slice` เป็นเพียงวิธีการพูดในแบบเมธอด สำหรับ regex, `/[Rr]uby/` เป็นเหมือนกับการพูดว่า "จับ 'Ruby' หรือ 'ruby' ที่เจอก่อน" `split` จะแบ่งสตริงออกเป็นอาร์เรย์ทุกๆช่องว่าง และ `[2]` เลือกคำที่สาม—อาร์เรย์เริ่มที่ศูนย์ จำไว้นะ

มีตัวเลือกอื่นอีกไหม? แน่นอน, Ruby มีตัวเลือก `partition`, `rpartition`, และ `match` ก็สามารถใช้ที่นี่ได้ แต่ละตัวมีกรณีใช้งานของมันเอง แต่การรู้จัก `.slice` และ regex ก็ครอบคลุมส่วนใหญ่

สรุป: การดึงข้อความย่อยเกี่ยวกับการจัดการข้อความอย่างแม่นยำ เครื่องมือที่ถูกต้องหมายถึงโค้ดที่สะอาดและมีประสิทธิภาพ

## ดูเพิ่มเติม
- เอกสาร Ruby ในเรื่อง String: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- นิพจน์ประจำกาลใน Ruby: [ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- คู่มือสไตล์ Ruby ในเรื่อง Strings: [rubystyle.guide/#strings](https://rubystyle.guide/#strings)
