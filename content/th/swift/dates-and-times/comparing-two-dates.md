---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:51.472280-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E16\u0E32\
  \u0E21\u0E27\u0E48\u0E32 \"\u0E44\u0E01\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E02\
  \u0E48\u0E2D\u0E30\u0E44\u0E23\u0E21\u0E32\u0E01\u0E48\u0E2D\u0E19\u0E01\u0E31\u0E19\
  ?\" \u0E41\u0E15\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E43\u0E19\u0E1B\u0E0F\u0E34\u0E17\u0E34\u0E19 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E40\u0E23\u0E35\
  \u0E22\u0E07\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C,\u2026"
lastmod: '2024-03-17T21:57:56.573560-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E16\u0E32\
  \u0E21\u0E27\u0E48\u0E32 \"\u0E44\u0E01\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E02\
  \u0E48\u0E2D\u0E30\u0E44\u0E23\u0E21\u0E32\u0E01\u0E48\u0E2D\u0E19\u0E01\u0E31\u0E19\
  ?\" \u0E41\u0E15\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E43\u0E19\u0E1B\u0E0F\u0E34\u0E17\u0E34\u0E19 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E40\u0E23\u0E35\
  \u0E22\u0E07\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C,\u2026"
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## อะไร & ทำไม?
การเปรียบเทียบวันที่สองวันเหมือนกับการถามว่า "ไก่หรือไข่อะไรมาก่อนกัน?" แต่เป็นกับวันที่ในปฏิทิน โปรแกรมเมอร์ทำการนี้เพื่อจัดเรียงเหตุการณ์, กระตุ้นการกระทำ, และประเมินช่วงเวลา

## วิธีทำ:
Swift ใช้ `Date` สำหรับวันที่และเวลา นี่คือวิธีง่ายๆในการเปรียบเทียบวันที่สองวัน:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

// สร้างวัตถุวันที่สองอัน
let date1 = dateFormatter.date(from: "2023/01/01 09:00")!
let date2 = dateFormatter.date(from: "2023/02/01 10:00")!

// เปรียบเทียบวันที่
if date1 == date2 {
    print("วันที่เหมือนกัน")
} else if date1 < date2 {
    print("วันที่1 อยู่ก่อนวันที่2")
} else {
    print("วันที่1 อยู่หลังจากวันที่2")
}
```

ตัวอย่างผลลัพธ์:

`วันที่1 อยู่ก่อนวันที่2`

สามารถใช้โอเปอเรเตอร์เปรียบเทียบได้เพราะ `Date` สอดคล้องกับโปรโตคอล `Comparable`

## ลงลึก:
วันที่ไม่เคยมาในรูปแบบวัตถุที่สะดวกเสมอไป ในต้นแต่คุณต้องจัดการกับส่วนประกอบแต่ละอย่าง เช่น ปี, เดือน, และวัน ยากกว่ามาก ตอนนี้ วัตถุ `Date` ใน Swift รับภาระหนัก, และการเปรียบเทียบพวกมันเป็นเรื่องตรงไปตรงมาด้วยโอเปอเรเตอร์ที่ให้มา

ก่อน Swift และ `Date` ของ Cocoa, Objective-C ใช้ `NSDate`, แต่มันสามารถเชื่อมต่อได้, ดังนั้นโค้ดเก่ายังสามารถใช้งานร่วมกันได้

และเฮ้, ไม่ได้มีแค่ `<`, `>`, และ `==` — คุณยังสามารถใช้ `timeIntervalSince(_:)` สำหรับการควบคุมที่ละเอียดกว่า, เช่น:

```Swift
let timeInterval = date2.timeIntervalSince(date1)
```

นี่ให้คุณค่าต่างเป็นวินาที ค่าบวก: date2 อยู่ข้างหน้า; ค่าลบ: อยู่ข้างหลัง; ศูนย์: พวกมันเหมือนกัน มีประโยชน์มากสำหรับตัวจับเวลา, การนับถอยหลัง, และการติดตามระยะเวลา ในเชิงลึก, วันที่เป็นเพียงจุดอ้างอิงในเวลา—คิดถึงพวกมันเป็นเหมือนตราประทับเวลาที่หรูหรา

## ดูเพิ่มเติม:
- Apple's Date documentation: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- คำแนะนำการจัดรูปแบบวันที่: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)
