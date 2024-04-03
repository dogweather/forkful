---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:10.896964-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07 (Parsing a date from a string) \u0E40\u0E01\u0E35\u0E48\u0E22\
  \u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\
  \u0E07\u0E01\u0E32\u0E23\u0E41\u0E17\u0E19\u0E04\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E17\u0E35\u0E48\u0E40\u0E1B\
  \u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E2D\
  \u0E47\u0E2D\u0E1A\u0E40\u0E08\u0E01\u0E15\u0E4C `Date`\u2026"
lastmod: '2024-03-17T21:57:56.570813-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07 (Parsing a date from a string) \u0E40\u0E01\u0E35\u0E48\u0E22\
  \u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\
  \u0E07\u0E01\u0E32\u0E23\u0E41\u0E17\u0E19\u0E04\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E17\u0E35\u0E48\u0E40\u0E1B\
  \u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E2D\
  \u0E47\u0E2D\u0E1A\u0E40\u0E08\u0E01\u0E15\u0E4C `Date` \u0E01\u0E23\u0E30\u0E1A\
  \u0E27\u0E19\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\
  \u0E48\u0E07\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E43\u0E19\u0E41\u0E2D\u0E1B\u0E1E\
  \u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E16\u0E39\u0E01\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E40\u0E1B\
  \u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E40\u0E0A\u0E48\u0E19 \u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E15\u0E2D\u0E1A\u0E01\u0E25\u0E31\u0E1A API \u0E2B\u0E23\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E1B\u0E49\u0E2D\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E08\u0E32\u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49 \u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E41\u0E25\
  \u0E30\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19."
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## วิธีการ:


### การใช้ `DateFormatter` ของ Foundation
ไลบรารีมาตรฐานของ Swift คือ Foundation มี `DateFormatter` สำหรับการแปลงสตริงเป็นอ็อบเจกต์ `Date` และในทางกลับกัน เพื่อแยกวิเคราะห์วันที่จากสตริง คุณต้องระบุรูปแบบวันที่ที่ตรงกับสตริง จากนั้นใช้ฟอร์แมตเตอร์เพื่อแยกวิเคราะห์มัน

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("วันที่ที่แยกวิเคราะห์ได้: \(date)")
} else {
    print("ไม่สามารถแยกวิเคราะห์วันที่ได้")
}
// ตัวอย่างผลลัพธ์: วันที่ที่แยกวิเคราะห์ได้: 2023-04-29 22:00:00 +0000
```

โปรดทราบว่าผลลัพธ์อาจแตกต่างกันขึ้นอยู่กับเขตเวลาของคุณ

### การใช้ ISO8601DateFormatter
สำหรับรูปแบบวันที่ ISO 8601, Swift มีฟอร์แมตเตอร์เฉพาะ `ISO8601DateFormatter`, ซึ่งทำให้กระบวนการแยกวิเคราะห์ง่ายขึ้น

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("วันที่ ISO8601 ที่แยกวิเคราะห์ได้: \(date)")
} else {
    print("ไม่สามารถแยกวิเคราะห์วันที่ ISO8601 ได้")
}
// ตัวอย่างผลลัพธ์: วันที่ ISO8601 ที่แยกวิเคราะห์ได้: 2023-04-30 15:00:00 +0000
```

### การใช้ไลบรารีภายนอก: SwiftDate
แม้ว่า Swift จะมีเครื่องมือที่เข้มแข็งสำหรับการแยกวิเคราะห์วันที่ แต่ไลบรารีภายนอกเช่น SwiftDate มอบความยืดหยุ่นและความสะดวกในการใช้งานที่ยิ่งขึ้น หลังจากเพิ่ม SwiftDate เข้ากับโปรเจกต์ของคุณ การแยกวิเคราะห์จะเป็นเรื่องง่ายเช่น:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("วันที่ที่แยกวิเคราะห์ได้ด้วย SwiftDate: \(date)")
} else {
    print("ไม่สามารถแยกวิเคราะห์วันที่ด้วย SwiftDate ได้")
}
// ตัวอย่างผลลัพธ์: วันที่ที่แยกวิเคราะห์ได้ด้วย SwiftDate: 2023-04-30 00:00:00 +0000
```

SwiftDate ทำให้การแยกวิเคราะห์เป็นเรื่องง่ายด้วยภาษาธรรมชาติและรูปแบบวันที่ที่หลากหลาย ทำให้เป็นเครื่องมือที่ทรงพลังในการเขียนโปรแกรม Swift ของคุณ
