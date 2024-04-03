---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:52.417776-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Swift \u0E43\u0E0A\u0E49\
  \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\
  \u0E14\u0E1E\u0E25\u0E32\u0E14\u0E14\u0E49\u0E27\u0E22\u0E1A\u0E25\u0E47\u0E2D\u0E01\
  \ `do`, `try`, \u0E41\u0E25\u0E30 `catch` \u0E21\u0E32\u0E14\u0E39\u0E01\u0E31\u0E19\
  ."
lastmod: '2024-03-17T21:57:56.568949-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0E43\u0E0A\u0E49\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E14\u0E49\u0E27\
  \u0E22\u0E1A\u0E25\u0E47\u0E2D\u0E01 `do`, `try`, \u0E41\u0E25\u0E30 `catch` \u0E21\
  \u0E32\u0E14\u0E39\u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## วิธีการ:
Swift ใช้การจัดการข้อผิดพลาดด้วยบล็อก `do`, `try`, และ `catch` มาดูกัน:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // ลองนึกว่าเรามีโลจิคที่นี่เพื่อตรวจสอบว่าไฟล์มีอยู่หรือไม่ และเรามีสิทธิ์ในการอ่านหรือไม่
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "เนื้อหาไฟล์อยู่ที่นี่"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("อุ๊ปส์! ไม่พบไฟล์")
} catch FileError.noPermission {
    print("อ๋อ! ไม่มีสิทธิ์ในการอ่านไฟล์")
} catch {
    print("เกิดข้อผิดพลาดที่ไม่รู้จัก")
}

```

ตัวอย่างผลลัพธ์:

```
อุ๊ปส์! ไม่พบไฟล์
```

## ศึกษาลึกลงไป
การจัดการข้อผิดพลาดไม่เคยเป็นเรื่องง่ายอย่างที่เป็นอยู่ในตอนนี้ เมื่อใช้ Objective-C, คุณจะต้องจัดการกับการชี้ไปยังวัตถุ NSError ซึ่งรู้สึกอึดอัด ตอนนี้เรามีระบบที่สง่างามกว่าด้วย Swift enums และโปรโตคอล `Error`

`throw` ของ Swift ช่วยให้เราสามารถส่งสัญญาณว่ามีบางอย่างผิดปกติได้ บล็อก `do` ทำหน้าที่เหมือนพื้นที่ที่ตระหนักถึงข้อผิดพลาด, เครื่องหมาย `try` เรียกธุรกิจที่มีความเสี่ยง, และ `catch` จัดการกับสิ่งต่างๆหากพวกเขาพาดพิง

Optional เป็นทางเลือกสำหรับสถานการณ์ที่ไม่ถึงกับเป็น "สถานะข้อผิดพลาด" แต่อาจยังไม่มี "ผลลัพธ์" ประการหนึ่ง เหมือนกับตัวแปรของชเรอดิงเกอร์—มีค่าหรือไม่มีค่าก็ได้

สำหรับการศึกษาลึกยิ่งขึ้น, ตรวจสอบประเภท `Result` ซึ่งเป็นผสมผสานที่น่าสนใจระหว่างรูปแบบการคืนค่าปกติและรูปแบบข้อผิดพลาด

## ดูเพิ่มเติม
- คู่มือการจัดการข้อผิดพลาดของ Swift อย่างเป็นทางการ: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- วิธีการที่ดีที่สุดในการจัดการข้อผิดพลาดของ Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- การจัดการข้อผิดพลาดขั้นสูงใน Swift: [บทความ Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
