---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:55.405075-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ \u0E43\u0E19 Swift \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E1A\u0E32\u0E41\
  \u0E25\u0E30\u0E40\u0E2B\u0E21\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49 JSON\u2026"
lastmod: '2024-03-17T21:57:56.581975-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ \u0E43\u0E19 Swift \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E1A\u0E32\u0E41\
  \u0E25\u0E30\u0E40\u0E2B\u0E21\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49 JSON\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## อะไร & ทำไม?

การทำงานกับ JSON ใน Swift หมายถึงการจัดการกับรูปแบบข้อมูลที่เบาและเหมาะสำหรับการแลกเปลี่ยนข้อมูล โปรแกรมเมอร์ใช้ JSON เพื่อส่งข้อมูลระหว่างเซิร์ฟเวอร์และเว็บแอปพลิเคชันเนื่องจากมันอ่านง่ายและง่ายต่อการแยกข้อมูลสำหรับทั้งมนุษย์และเครื่องจักร

## วิธีการ:

Swift ทำให้การแยกวิเคราะห์ JSON เป็นเรื่องง่ายด้วยโปรโตคอล `Codable` นี่คือวิธีที่คุณถอดรหัส JSON เป็นอ็อบเจกต์ Swift:

```Swift
import Foundation

// กำหนดโมเดลที่อยู่ในรูปแบบของ Codable
struct User: Codable {
    var name: String
    var age: Int
}

// สตริง JSON
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// แปลงสตริง JSON เป็น Data
if let jsonData = jsonString.data(using: .utf8) {
    // ถอดรหัสข้อมูล JSON เป็นอ็อบเจกต์ User
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Name: \(user.name), Age: \(user.age)")
    } catch {
        print("ข้อผิดพลาดในการถอดรหัส JSON: \(error)")
    }
}
```

ตัวอย่างผลลัพธ์:
```
Name: John Doe, Age: 30
```

## การขุดลึก

JSON (JavaScript Object Notation) ได้รับความนิยมอย่างกว้างขวางตั้งแต่ต้นทศวรรษ 2000 หลังจากที่ Douglas Crockford ได้กำหนดข้อกำหนด มันได้แทนที่ XML สำหรับหลาย ๆ เคสการใช้งานเนื่องจากมันมีไวยากรณ์ที่ง่ายกว่าและประสิทธิภาพที่ดีกว่า ในขณะที่ `Codable` ของ Swift เป็นที่นิยมสำหรับ JSON, ตัวเลือกรายการอื่น ๆ เช่น `JSONSerialization` ยังมีอยู่สำหรับการจัดการกับประเภทที่ไม่เข้ากับมาตรฐานของ Codable ด้วยการทำงานภายใน, `Codable` ทำให้การแยกและรวมข้อมูลเป็นไปอย่างราบรื่น

## ดูเพิ่มเติม

- สำรวจเพิ่มเติมเกี่ยวกับ JSON และ Swift ในบล็อกอย่างเป็นทางการของ Swift: [Swift.org](https://swift.org/blog/)
- ดูเอกสารของ `Codable`: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- สำหรับโครงสร้าง JSON ที่ซับซ้อน, พิจารณาไลบรารีของบุคคลที่สาม เช่น SwiftyJSON ที่มีให้บริการที่ [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
