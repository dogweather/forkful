---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:29.454041-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07 Ruby \u0E21\
  \u0E35\u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\u0E32\u0E22\u0E46\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E2B\u0E23\
  \u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\
  \u0E35\u0E17\u0E33\u0E14\u0E49\u0E27\u0E22 Ruby \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\u0E43\u0E14\u0E46."
lastmod: '2024-03-17T21:57:56.743197-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19\u0E02\u0E2D\u0E07 Ruby \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\
  \u0E32\u0E22\u0E46\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\
  \u0E35\u0E2D\u0E22\u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33\u0E14\u0E49\u0E27\u0E22\
  \ Ruby \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\u0E43\
  \u0E14\u0E46."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีทำ:
ไลบรารีมาตรฐานของ Ruby มีวิธีง่ายๆในการตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ นี่คือวิธีทำด้วย Ruby โดยไม่ต้องใช้ไลบรารีภายนอกใดๆ:

```ruby
require 'fileutils'

# ตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่
if Dir.exist?('/path/to/directory')
  puts 'ไดเรกทอรีมีอยู่.'
else
  puts 'ไดเรกทอรีไม่มีอยู่.'
end
```
ตัวอย่างผลลัพธ์:
```
ไดเรกทอรีมีอยู่.
```
หรือ:
```
ไดเรกทอรีไม่มีอยู่.
```

นอกจากการใช้ `Dir.exist?` คุณยังสามารถใช้วิธี `File.directory?` ซึ่งจะคืนค่า `true` ถ้าพาทที่ให้มาเป็นไดเรกทอรี:

```ruby
if File.directory?('/path/to/directory')
  puts 'ไดเรกทอรีมีอยู่.'
else
  puts 'ไดเรกทอรีไม่มีอยู่.'
end
```
ทั้ง `Dir.exist?` และ `File.directory?` เป็นส่วนหนึ่งของไลบรารีมาตรฐานของ Ruby และไม่ต้องการ gems ภายนอกใดๆเพื่อใช้งาน ทำให้เป็นตัวเลือกที่สะดวกและมีประสิทธิภาพสำหรับการตรวจสอบไดเรกทอรี
