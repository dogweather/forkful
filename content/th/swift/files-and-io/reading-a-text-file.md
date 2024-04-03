---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:38.806324-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E2D\u0E48\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E08\u0E32\u0E01\
  \u0E44\u0E1F\u0E25\u0E4C\u0E43\u0E19 Swift \u0E43\u0E2B\u0E49\u0E43\u0E0A\u0E49\u0E40\
  \u0E21\u0E18\u0E2D\u0E14\u0E17\u0E35\u0E48\u0E2A\u0E30\u0E14\u0E27\u0E01\u0E02\u0E2D\
  \u0E07\u0E04\u0E25\u0E32\u0E2A `String` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E02\u0E19\u0E32\u0E14\u0E40\u0E25\u0E47\
  \u0E01."
lastmod: '2024-03-17T21:57:56.578220-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E48\u0E32\u0E19\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E08\u0E32\u0E01\u0E44\u0E1F\u0E25\u0E4C\u0E43\u0E19 Swift\
  \ \u0E43\u0E2B\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14\u0E17\u0E35\
  \u0E48\u0E2A\u0E30\u0E14\u0E27\u0E01\u0E02\u0E2D\u0E07\u0E04\u0E25\u0E32\u0E2A `String`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E02\u0E19\u0E32\u0E14\u0E40\u0E25\u0E47\u0E01."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## วิธีทำ:
เพื่ออ่านข้อความจากไฟล์ใน Swift ให้ใช้เมธอดที่สะดวกของคลาส `String` นี่คือตัวอย่างขนาดเล็ก:

```Swift
import Foundation

if let filePath = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let content = try String(contentsOfFile: filePath, encoding: .utf8)
        print(content)
    } catch {
        print("อุ๊ปส์! มีบางอย่างผิดพลาด: \(error)")
    }
}
```
หาก "example.txt" มีข้อความว่า "Hello, world!" ผลลัพธ์ที่ได้คือ:
```
Hello, world!
```

## ลงลึก
การอ่านไฟล์ข้อความเป็นเรื่องเก่าแก่ในโลกการเขียนโปรแกรม ตั้งแต่ยุคแรกๆ มันเกี่ยวข้องกับการ์ดเจาะรูและเทป ตอนนี้ ด้วยภาษาระดับสูงเช่น Swift มันง่ายขึ้นมาก ซนิปเปตข้างต้นใช้ `String(contentsOfFile:)`, แต่ยังมีทางเลือกอื่นๆ:

- `FileManager`: ดีสำหรับการดำเนินการกับไฟล์ที่ซับซ้อนมากขึ้น
- `InputStream`: ใช้เมื่อต้องจัดการกับไฟล์ขนาดใหญ่—ใช้หน่วยความจำน้อยกว่า
- `URLSession`: ดึงไฟล์จากเซิร์ฟเวอร์ระยะไกล

การใช้งาน `String(contentsOfFile:)` อาจใช้หน่วยความจำเยอะหากใช้กับไฟล์ขนาดใหญ่ ในการป้องกันปัญหา พิจารณาใช้วิธีการใช้สตรีมหรือการอ่านแบบมีชั้น

## ดูเพิ่มเติม
หาข้อมูลเพิ่มเติมในเอกสารการใช้งานเป็นทางการของ Swift:
- [String](https://developer.apple.com/documentation/swift/string)
- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [การทำงานกับ URL Session](https://developer.apple.com/documentation/foundation/url_loading_system/fetching_website_data_into_memory)

สำหรับการเข้าใจอย่างลึกซึ้ง ตรวจสอบแหล่งข้อมูลเหล่านี้:
- [คู่มือการเขียนโปรแกรมไฟล์ระบบของ Apple](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
