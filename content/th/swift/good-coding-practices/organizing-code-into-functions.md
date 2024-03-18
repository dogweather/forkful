---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:21.816039-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E25\u0E38\u0E48\u0E21\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\u0E07\
  \u0E32\u0E19\u0E2D\u0E2D\u0E01\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E17\
  \u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E44\u0E14\u0E49\
  \u0E0B\u0E49\u0E33 \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\u0E04\u0E49\u0E14\
  \u0E2A\u0E30\u0E2D\u0E32\u0E14 \u0E25\u0E14\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\
  \u0E1E\u0E25\u0E32\u0E14 \u0E41\u0E25\u0E30\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E2B\u0E23\u0E37\u0E2D\u0E23\u0E35\
  \u0E41\u0E1F\u0E04\u0E40\u0E15\u0E2D\u0E23\u0E4C"
lastmod: '2024-03-17T21:57:56.567082-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E25\u0E38\u0E48\u0E21\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\u0E07\
  \u0E32\u0E19\u0E2D\u0E2D\u0E01\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E17\
  \u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E44\u0E14\u0E49\
  \u0E0B\u0E49\u0E33 \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\u0E04\u0E49\u0E14\
  \u0E2A\u0E30\u0E2D\u0E32\u0E14 \u0E25\u0E14\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\
  \u0E1E\u0E25\u0E32\u0E14 \u0E41\u0E25\u0E30\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E2B\u0E23\u0E37\u0E2D\u0E23\u0E35\
  \u0E41\u0E1F\u0E04\u0E40\u0E15\u0E2D\u0E23\u0E4C"
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การจัดกลุ่มคำสั่งเป็นฟังก์ชันคือการแบ่งงานออกเป็นส่วนที่สามารถใช้ได้ซ้ำ ช่วยให้โค้ดสะอาด ลดความผิดพลาด และง่ายต่อการดีบักหรือรีแฟคเตอร์

## วิธีการ:
จินตนาการถึงงานหนึ่ง: คำนวณค่าเฉลี่ยของอาร์เรย์ หากไม่มีฟังก์ชัน คุณจะใส่ลงไปทั้งหมดใน main แต่ด้วยฟังก์ชัน คุณจะทำแบบนี้:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// การใช้
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("ค่าเฉลี่ยคะแนนคือ \(averageScore)")
```

ผลลัพธ์ตัวอย่างจะเป็น:
```
ค่าเฉลี่ยคะแนนคือ 87.6875
```

## ลงลึก
ตามประวัติศาสตร์ เมื่อการเขียนโปรแกรมกลายเป็นเรื่องซับซ้อนขึ้น ฟังก์ชันกลายเป็นหัวใจสำคัญในการจัดการความซับซ้อน ทางเลือกอื่น ๆ ได้แก่ การเขียนโค้ดในบรรทัดเดียวและการคัดลอกโค้ด (สปาเก็ตตี้โค้ด) - ซึ่งปัจจุบันถือว่าเป็นปฏิบัติการไม่ดี ใน Swift, ฟังก์ชันเป็นสิ่งสำคัญ; สามารถกำหนดให้กับตัวแปร ส่งผ่านเป็นอาร์กิวเมนต์ และส่งคืนจากฟังก์ชันอื่น ส่งผลให้โค้ดมีความยืดหยุ่นและปรับตัวได้มากขึ้น

ในเชิงการดำเนินการ ออกแบบฟังก์ชันของคุณให้ทำสิ่งหนึ่งได้ดี มีเป้าหมายที่ชัดเจนและชื่อที่สะท้อนถึงสิ่งนั้น พิจารณาจำนวนพารามิเตอร์—หากมีมากเกินไป คุณอาจทำงานมากเกินไป การจัดการข้อผิดพลาด? พิจารณาใช้ฟังก์ชันการโยนและจัดการปัญหาอย่างสง่างาม จำไว้ว่า: Swift เกี่ยวข้องกับความสามารถในการอ่านและความง่ายในการบำรุงรักษา

## ดูเพิ่มเติม
- [คู่มือภาษาการเขียนโปรแกรม Swift - ฟังก์ชัน](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [คู่มือสไตล์ Swift ของ Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [การปรับโครงสร้างโดย Martin Fowler: การปรับปรุงการออกแบบของโค้ดที่มีอยู่](https://martinfowler.com/books/refactoring.html)
