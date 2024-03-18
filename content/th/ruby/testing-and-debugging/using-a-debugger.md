---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:49.039829-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E14\u0E35\u0E1A\
  \u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E43\u0E19 Ruby \u0E43\u0E2B\u0E49\u0E1E\
  \u0E25\u0E31\u0E07\u0E1E\u0E34\u0E40\u0E28\u0E29\u0E01\u0E31\u0E1A\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E2B\u0E22\u0E38\u0E14\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E15\u0E19,\
  \ \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  , \u0E41\u0E25\u0E30\u0E40\u0E14\u0E34\u0E19\u0E17\u0E32\u0E07\u0E1C\u0E48\u0E32\
  \u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E15\u0E19\u0E1A\u0E23\u0E23\u0E17\
  \u0E31\u0E14\u0E15\u0E48\u0E2D\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u2026"
lastmod: '2024-03-17T21:57:56.733680-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E14\u0E35\u0E1A\
  \u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E43\u0E19 Ruby \u0E43\u0E2B\u0E49\u0E1E\
  \u0E25\u0E31\u0E07\u0E1E\u0E34\u0E40\u0E28\u0E29\u0E01\u0E31\u0E1A\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E2B\u0E22\u0E38\u0E14\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E15\u0E19,\
  \ \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  , \u0E41\u0E25\u0E30\u0E40\u0E14\u0E34\u0E19\u0E17\u0E32\u0E07\u0E1C\u0E48\u0E32\
  \u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E15\u0E19\u0E1A\u0E23\u0E23\u0E17\
  \u0E31\u0E14\u0E15\u0E48\u0E2D\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การใช้งานดีบักเกอร์ใน Ruby ให้พลังพิเศษกับโปรแกรมเมอร์ในการหยุดโค้ดของตน, ตรวจสอบตัวแปร, และเดินทางผ่านโค้ดของตนบรรทัดต่อบรรทัด ผู้คนทำเช่นนี้เพื่อบีบบังคับแมลง, เข้าใจการไหลของโค้ด, และเพื่อดูอย่างแม่นยำว่าคำสั่งที่เขียนของพวกเขา (โค้ด) กำลังทำอะไรเมื่อเกิดเหตุการณ์วิเศษ - หรือไม่เกิดเหตุการณ์วิเศษ

## วิธีการ:

Ruby มาพร้อมกับดีบักเกอร์ในตัวที่เรียกว่า `byebug` ขั้นแรก, ใส่ `byebug` ในไฟล์ Gemfile ของคุณและรัน `bundle install` จากนั้น, วาง `byebug` ตรงที่คุณต้องการให้โปรแกรมของคุณหยุดพัก

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

การรันสคริปต์นี้จะหยุดการทำงานที่ `byebug`, และคุณจะถูกนำไปยังเซสชันโต้ตอบที่คุณสามารถพิมพ์คำสั่งเช่น:

```
step
next
continue
var local
```

ตัวอย่างของการแสดงผลจะดูเป็นประมาณนี้:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## ศึกษาลึกซึ้ง:

ก่อนมี `byebug`, Rubyists ใช้ `debugger` และ `pry` เจ้าหลัง, `pry`, ไม่ใช่แค่ดีบักเกอร์; มันเป็น REPL ที่ทรงพลังที่สามารถใช้สำหรับการดีบักได้ด้วยจุดหยุด `binding.pry`

ทางเลือกอื่นสำหรับ `byebug` ของ Ruby รวมถึง `pry-byebug`, ซึ่งผสมผสาน `pry` กับฟังก์ชัน `byebug`, และ `ruby-debug`, ซึ่งเป็น gem เก่าที่ไม่ได้รับการดูแลอัพเดทอีกต่อไป

เมื่อคุณเรียกใช้ `byebug`, ดีบักเกอร์จะหยุดการดำเนินการของโค้ดและให้คุณดูเข้าไปในเวลาทำงานได้ คุณสามารถดูและเปลี่ยนแปลงตัวแปร, กระโดดไปยังจุดต่างๆ ในโค้ด, และแม้กระทั่งรันบางส่วนของโค้ด Ruby บรรทัดต่อบรรทัด มันเหมือนกับการมีความสามารถเดินทางข้ามเวลาสำหรับโค้ด Ruby ของคุณ

## ดูเพิ่มเติมที่:

- โครงการ GitHub ของ Byebug: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- เอกสารของ Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- คู่มือในการดีบักแอปพลิเคชัน Rails: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
