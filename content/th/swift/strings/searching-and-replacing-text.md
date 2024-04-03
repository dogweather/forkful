---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:35.732488-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:56.547675-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
```Swift
var greetings = "Hello, old friend!"

// แทนที่แบบง่าย
greetings = greetings.replacingOccurrences(of: "old", with: "new")
print(greetings) // "Hello, new friend!"

// การใช้ตัวเลือกสำหรับการแทนที่โดยไม่คำนึงถึงตัวพิมพ์ใหญ่เล็ก
let caseInsensitiveResult = greetings.replacingOccurrences(
    of: "hello",
    with: "Hi",
    options: .caseInsensitive
)
print(caseInsensitiveResult) // "Hi, new friend!"

// การแทนที่ด้วย Regular Expressions
let regexResult = greetings.replacingOccurrences(
    of: "\\bnew\\b",
    with: "best",
    options: .regularExpression
)
print(regexResult) // "Hello, best friend!"
```

## ลงลึก
เราได้ทำการสลับข้อความในสตริงมาตั้งแต่ยุคแรกๆ ของการคอมพิวต์ เดิมที, มันเกิดขึ้นกับเครื่องมือ Command-line ง่ายๆ เช่น `sed` ใน Swift, `replacingOccurrences(of:with:)` ทำหน้าที่หนัก, และคุณได้รับการควบคุมมากขึ้นด้วยตัวเลือกเช่น `.caseInsensitive` หรือ `.regularExpression`

ทางเลือกอื่นๆ ใน Swift ได้แก่การใช้ `NSRegularExpression` สำหรับรูปแบบที่ซับซ้อนและ `NSMutableString` สำหรับการดำเนินการสตริงที่เปลี่ยนแปลงได้ ในขั้นต้น, วิธีการแทนที่ข้อความของ Swift ทำงานร่วมกับเคาน์เตอร์พาร์ท Objective-C ที่มีประสิทธิภาพ, ให้ความเร็วและความหลากหลาย

## ดูเพิ่มเติม
- [เอกสาร Swift String](https://developer.apple.com/documentation/swift/string/)
- [Regular Expressions ใน Swift](https://nshipster.com/swift-regular-expressions/)
- [Swift.org - การทำงานกับ Strings](https://swift.org/documentation/api-design-guidelines/#strive-for-fluent-usage)
