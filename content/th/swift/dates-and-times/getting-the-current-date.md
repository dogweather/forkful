---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:29.627015-06:00
description: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19 Swift \u0E19\u0E31\u0E49\
  \u0E19\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `Date` \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E17\u0E35\u0E48\u0E41\u0E2D\
  \u0E1E\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E16\u0E39\u0E01\u0E40\u0E23\
  \u0E35\u0E22\u0E01\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.571747-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19 Swift \u0E19\u0E31\u0E49\
  \u0E19\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `Date` \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E17\u0E35\u0E48\u0E41\u0E2D\
  \u0E1E\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E16\u0E39\u0E01\u0E40\u0E23\
  \u0E35\u0E22\u0E01\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การรับวันที่ปัจจุบันใน Swift นั้นเกี่ยวข้องกับการใช้คลาส `Date` เพื่อเข้าถึงวันที่และเวลาที่แอพพลิเคชันถูกเรียกใช้งาน โปรแกรมเมอร์ต้องการดึงวันที่ปัจจุบันสำหรับเหตุผลหลายประการตั้งแต่การติดเวลาในเหตุการณ์, การคำนวณวันที่, ไปจนถึงการแสดงวันที่และเวลาในหน้าต่างผู้ใช้

## วิธีการ:
เฟรมเวิร์ก `Foundation` ของ Swift มีการให้บริการคลาส `Date` ทำให้สามารถรับวันที่และเวลาปัจจุบันได้อย่างง่ายดาย นี่คือตัวอย่างพื้นฐานของวิธีการรับวันที่ปัจจุบัน:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

สิ่งนี้จะมีการแสดงผลออกมาประมาณ:

```
2023-04-12 07:46:23 +0000
```

รูปแบบผลลัพธ์ตามมาตรฐาน ISO 8601 โดยใช้เขตเวลา UTC อย่างไรก็ตาม คุณอาจต้องการจัดรูปแบบวันที่นี้เพื่อจุดประสงค์ในการแสดงผล คลาส `DateFormatter` ของ Swift จึงเข้ามาช่วยเหลือ:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

ตัวอย่างผลลัพธ์อาจเป็น:

```
12 เมษายน 2023 เวลา 10:46:23 น.
```

โปรดทราบว่ารูปแบบผลลัพธ์จะแตกต่างกันไปขึ้นอยู่กับสถานที่ของอุปกรณ์ที่รันโค้ด

สำหรับโปรเจกต์ที่ต้องการการจัดการวันที่ที่ซับซ้อนมากกว่า นักพัฒนา Swift หลายคนหันไปใช้ไลบรารีของบุคคลที่สาม เช่น `SwiftDate` นี่คือวิธีที่คุณอาจใช้ `SwiftDate` เพื่อรับวันที่ปัจจุบันในเขตเวลาและรูปแบบที่ระบุ:

ก่อนอื่น ให้เพิ่ม `SwiftDate` เข้าในโปรเจกต์ของคุณโดยใช้ SPM, CocoaPods, หรือ Carthage จากนั้น:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

สิ่งนี้อาจให้ผลลัพธ์ออกมาว่า:

```
2023-04-12 09:46:23
```

ใช้ `SwiftDate`, คุณสามารถจัดการกับวันที่และเวลาได้อย่างง่ายดายสำหรับเขตเวลาและสถานที่ต่างๆ ทำให้งานด้านการจัดการวันที่ที่ซับซ้อนในแอพพลิเคชัน Swift ของคุณง่ายขึ้น
