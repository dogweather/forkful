---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:54.295630-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Swift, \u0E44\
  \u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\
  \u0E19\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E40\u0E14\u0E34\u0E21\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E44\u0E1F\u0E25\u0E4C CSV \u0E42\
  \u0E14\u0E22\u0E15\u0E23\u0E07 \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25 CSV \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 `String`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E40\u0E19\u0E37\u0E49\u0E2D\
  \u0E2B\u0E32\u2026"
lastmod: '2024-03-17T21:57:56.582861-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Swift, \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\
  \u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E40\u0E14\
  \u0E34\u0E21\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E44\u0E1F\u0E25\u0E4C CSV \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E41\u0E15\u0E48\
  \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 CSV \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \u0E40\u0E21\u0E18\u0E2D\u0E14 `String` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\
  \u0E01\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32 \u0E2B\u0E23\u0E37\u0E2D\u0E42\u0E14\
  \u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\
  \u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E40\u0E0A\u0E48\
  \u0E19 SwiftCSV \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\
  \u0E32\u0E16\u0E36\u0E07\u0E17\u0E35\u0E48\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E20\u0E32\u0E1E\u0E21\u0E32\u0E01\u0E02\u0E36\u0E49\u0E19 \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E31\u0E49\u0E07\u0E2A\
  \u0E2D\u0E07."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:
ใน Swift, ไม่มีการสนับสนุนเนื้อเดิมสำหรับการแยกไฟล์ CSV โดยตรง แต่คุณสามารถจัดการข้อมูล CSV โดยใช้เมธอด `String` เพื่อแยกเนื้อหา หรือโดยใช้ไลบรารีของบุคคลที่สามเช่น SwiftCSV เพื่อการเข้าถึงที่มีประสิทธิภาพมากขึ้น นี่คือวิธีทั้งสอง:

### การแยกวิเคราะห์ด้วยตัวเองโดยไม่ใช้ไลบรารี่ภายนอก
```swift
// พิจารณาสตริง CSV ที่เรียบง่าย
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// แยกสตริง CSV เป็นบรรทัด
let rows = csvString.components(separatedBy: "\n")

// สกัดคีย์จากบรรทัดแรก
let keys = rows.first?.components(separatedBy: ",")

// วนซ้ำผ่านบรรทัด เริ่มจากบรรทัดที่สอง
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// ผลลัพธ์ตัวอย่าง
print(result)
// แสดงผล: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
วิธีนี้ตรงไปตรงมาแต่ขาดความเข้มแข็ง โดยเฉพาะกับไฟล์ CSV ที่มีคดีพิเศษ เช่น จุลภาคในค่า, การขึ้นบรรทัดใหม่ภายในฟิลด์ ฯลฯ

### การใช้งานไลบรารี SwiftCSV
ขั้นต้น, เพิ่ม SwiftCSV เข้าไปในโปรเจคของคุณโดยการรวมไฟล์ใน `Package.swift` dependencies:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
จากนั้น, นำเข้าและใช้งานดังนี้:
```swift
import SwiftCSV

// ถือว่า `csvString` ถูกกำหนดไว้ตามที่กล่าวไว้ข้างต้น

// สร้างอ็อบเจกต์ CSV
if let csv = try? CSV(string: csvString) {
    // เข้าถึงแถวเป็นดิกชันนารี
    let rows = csv.namedRows
    
    // ผลลัพธ์ตัวอย่าง
    print(rows)
    // แสดงผล: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV ทำให้การแยกวิเคราะห์ง่ายขึ้นโดยการจัดการกับเฉดสีเช่น จุลภาคที่ถูกครอบคลุม, การขึ้นบรรทัดใหม่ในฟิลด์, และการเข้ารหัสตัวละครโดยอัตโนมัติ อย่างไรก็ตาม จำไว้ว่าต้องจัดการกับข้อผิดพลาดที่เป็นไปได้ในแอพพลิเคชั่นจริงๆ โดยเฉพาะเมื่อต้องจัดการกับแหล่งข้อมูลภายนอก
