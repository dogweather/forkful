---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:46.145498-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: structs `String` \u0E02\u0E2D\
  \u0E07 Swift \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E27\u0E34\
  \u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\
  \u0E36\u0E49\u0E19\u0E21\u0E32\u0E1A\u0E32\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E01\u0E23\
  \u0E13\u0E35\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E1A\u0E32\u0E07\u0E27\u0E34\
  \u0E18\u0E35\u0E43\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.545763-06:00'
model: gpt-4-0125-preview
summary: "structs `String` \u0E02\u0E2D\u0E07 Swift \u0E21\u0E32\u0E1E\u0E23\u0E49\
  \u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\
  \u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E36\u0E49\u0E19\u0E21\u0E32\u0E1A\u0E32\
  \u0E07\u0E2A\u0E48\u0E27\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E01\u0E31\u0E1A\u0E01\u0E23\u0E13\u0E35\u0E02\u0E2D\u0E07\u0E2A\u0E15\
  \u0E23\u0E34\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\
  \u0E32\u0E23\u0E1A\u0E32\u0E07\u0E27\u0E34\u0E18\u0E35\u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\
  \ Swift \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\
  \u0E41\u0E25\u0E30\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\
  \u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E2B\u0E32\u0E01\u0E08\
  \u0E33\u0E40\u0E1B\u0E47\u0E19\n\n#."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
structs `String` ของ Swift มาพร้อมกับวิธีการที่สร้างขึ้นมาบางส่วนเพื่อจัดการกับกรณีของสตริง นี่คือวิธีการบางวิธีในการทำให้สตริงเป็นตัวพิมพ์ใหญ่ใน Swift รวมถึงการใช้วิธีการมาตรฐานและไลบรารีของบุคคลที่สามหากจำเป็น

### การใช้วิธีการที่สร้างขึ้นมา
เพื่อทำให้ตัวอักษรแรกของสตริงเป็นตัวพิมพ์ใหญ่และเปลี่ยนตัวที่เหลือเป็นตัวพิมพ์เล็ก:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // ผลลัพธ์: "Hello, world"
```

เพื่อทำให้ตัวอักษรแรกของแต่ละคำในประโยคเป็นตัวพิมพ์ใหญ่ คุณสามารถใช้คุณสมบัติ `capitalized`:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // ผลลัพธ์: "Hello, World"
```

### การใช้ไลบรารีของบุคคลที่สาม
ในขณะที่ไลบรารีมาตรฐานของ Swift ค่อนข้างครอบคลุม รูปแบบการใช้ตัวพิมพ์ใหญ่บางแบบอาจต้องการการดำเนินการที่ซับซ้อนมากขึ้นหรือสามารถทำได้ง่ายขึ้นโดยใช้ไลบรารีของบุคคลที่สาม หนึ่งในไลบรารียอดนิยมสำหรับการจัดการสตริงคือ SwiftRichString (หมายเหตุ: ตรวจสอบเสมอว่าได้รวมไลบรารีของบุคคลที่สามผ่าน Swift Package Manager, CocoaPods หรือ Carthage และนำเข้าในไฟล์ของคุณ)

ก่อนอื่น คุณจะต้องเพิ่ม `SwiftRichString` เข้าในโปรเจ็กต์ของคุณ หลังจากติดตั้งเรียบร้อยแล้ว คุณสามารถใช้มันเพื่อดำเนินการต่างๆ กับสตริง รวมถึงความต้องการเฉพาะทางด้านการใช้ตัวพิมพ์ใหญ่ อย่างไรก็ตาม ณ ตอนนี้ วิธีการที่สร้างขึ้นมาใน Swift ครอบคลุมกรณีการใช้ตัวพิมพ์ใหญ่ส่วนใหญ่โดยไม่ต้องใช้ไลบรารีภายนอกรายอื่นสำหรับเพียงแค่ทำให้สตริงเป็นตัวพิมพ์ใหญ่

เสมอเข้าอ้างอิงเอกสารล่าสุดของไลบรารีเพื่ออัปเดตหรือเปลี่ยนแปลงวิธีการ
