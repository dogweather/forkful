---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:30.913651-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E19\u0E31\
  \u0E49\u0E19\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E43\u0E2A\
  \u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\u0E07\u0E17\u0E35\u0E48 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E1A\u0E1A\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\
  \u0E01\u2026"
lastmod: '2024-03-17T21:57:56.548627-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E19\u0E31\
  \u0E49\u0E19\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E43\u0E2A\
  \u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\u0E07\u0E17\u0E35\u0E48 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E1A\u0E1A\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\
  \u0E01\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การใส่ค่าตัวแปรลงในสตริงนั้นหมายถึงการใส่ตัวแปรเข้าไปในสตริงคงที่ โปรแกรมเมอร์ทำเช่นนี้เพื่อสร้างสตริงแบบไดนามิก ทำให้ง่ายขึ้นในการรวมข้อมูลตัวแปรเข้ากับผลลัพธ์ที่แสดงออกมา

## วิธีการ:
Swift ทำให้การเชื่อมต่อสตริงแบบนี้เป็นเรื่องง่ายด้วย syntax `\ (variableName)`.

```Swift
let name = "Jane"
let age = 28
let greeting = "Hello, \(name), you are \(age) years old."
print(greeting)  // ผลลัพธ์: Hello, Jane, you are 28 years old.
```

คุณยังสามารถทำการดำเนินการภายในการแทรกค่า:

```Swift
let apples = 3
let oranges = 5
let fruitSummary = "I have \(apples + oranges) pieces of fruit."
print(fruitSummary)  // ผลลัพธ์: I have 8 pieces of fruit.
```

## ลงลึก
โอเค, เรามาดูประวัติศาสตร์กันหน่อย String interpolation ไม่ใช่ที่เฉพาะของ Swift เท่านั้น มันมีอยู่ในหลายภาษา (เช่น JavaScript, Python, ฯลฯ) แต่เวอร์ชันของ Swift นั้นมีความปลอดภัยด้านประเภทข้อมูล หมายความว่า compiler จะตรวจสอบประเภทข้อมูลให้คุณ ลดความผิดพลาดได้

ก่อน Swift 5, string interpolation นั้นอาจดูไม่ทรงพลังและยุ่งยาก แต่ Swift 5 ได้แนะนำ Extended String Interpolation ซึ่งทำให้คุณสามารถตกแต่งการแทรกค่าสตริงได้ เพิ่มความยืดหยุ่นได้อย่างน่าประทับใจ

ทางเลือกอื่นๆ สำหรับการแทรกค่าสตริงใน Swift ได้แก่ การใช้ `+` สำหรับการต่อสตริง และ method `String(format:)` สไตล์เก่า อย่างไรก็ตาม วิธีเหล่านี้อาจไม่สะดวกเท่า และยากต่อการอ่านสำหรับสตริงรูปแบบ

รายละเอียดการนำไปใช้งาน? ด้วยระบบการแทรกค่าสตริงของ Swift คุณสามารถปรับแต่งวิธีการแสดงผลของพิมพ์ภายในสตริงได้โดยการขยายโปรโตคอล `StringInterpolation` นั่นหมายความว่าคุณสามารถกำหนดวิธีการแสดงผลของพิมพ์ที่กำหนดเองในระหว่างการแทรกค่า ซึ่งเป็นเรื่องที่สะดวกมาก

```Swift
extension String.StringInterpolation {
    mutating func appendInterpolation(_ value: Date) {
        let formatter = DateFormatter()
        formatter.dateStyle = .medium
        appendLiteral(formatter.string(from: value))
    }
}

let today = Date()
let dateString = "Today's date is \(today)."
print(dateString) // ผลลัพธ์จะเป็นวันที่ของวันนี้ในรูปแบบการจัดรูปแบบสไตล์กลาง
```

## ดูเพิ่มเติม
เพื่อรับรายละเอียดเพิ่มเติมเกี่ยวกับการแทรกค่าสตริง Swift's documentation นั้นมีประโยชน์อย่างยิ่ง:
- [การแทรกค่าสตริง](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [ข้อเสนอแนะการพัฒนาการแทรกค่าสตริงกับ Swift Evolution](https://github.com/apple/swift-evolution/blob/main/proposals/0228-fix-expressiblebystringinterpolation.md)

เพื่อขุดลึกลงไปยังการตกแต่งพิมพ์ที่กำหนดเอง:
- [การตกแต่งการแทรกค่าสตริงใน Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5)
