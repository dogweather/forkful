---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:34.543266-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Swift \u0E0A\
  \u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E08\u0E31\u0E14\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u0E44\u0E14\u0E49 \u0E2A\u0E34\u0E48\u0E07\
  \u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2A\u0E33\u0E04\
  \u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\
  \u0E07\u0E1C\u0E25\u0E1A\u0E19 UI, \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\
  \u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.572665-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Swift \u0E0A\
  \u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E08\u0E31\u0E14\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u0E44\u0E14\u0E49 \u0E2A\u0E34\u0E48\u0E07\
  \u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2A\u0E33\u0E04\
  \u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\
  \u0E07\u0E1C\u0E25\u0E1A\u0E19 UI, \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\
  \u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแปลงวันที่เป็นสตริงใน Swift ช่วยให้คุณจัดรูปแบบวันที่เพื่อมนุษย์ได้ สิ่งนี้เป็นสิ่งสำคัญสำหรับการแสดงผลบน UI, การบันทึก, หรือเมื่อใดก็ตามที่คุณต้องการให้วันที่สามารถเข้าใจได้โดยคน ไม่เพียงแต่โค้ดเท่านั้น

## วิธีการ:
Swift ใช้ `DateFormatter` เพื่อแปลงออบเจค `Date` เป็นสตริงที่อ่านได้ นี่คือวิธีการ:

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: date)
print(dateString) // ผลลัพธ์: "2023-04-05 14:20:35" (หรือวันและเวลาปัจจุบัน)
```

เปลี่ยน `dateFormat` เพื่อปรับแต่งว่าวันที่ของคุณจะดูเป็นอย่างไร:

```Swift
formatter.dateFormat = "EEEE, MMM d, yyyy"
print(formatter.string(from: date)) // ผลลัพธ์: "วันพุธ, เม.ย. 5, 2023"
```

## การทำความเข้าใจลึกซึ้ง
ก่อน `DateFormatter`, Objective-C และ Swift รุ่นแรกใช้ `NSDateFormatter`, ซึ่งพื้นฐานแล้วเป็นสิ่งเดียวกันที่ได้รับการเปลี่ยนแปลงชื่อ สิ่งสำคัญคือการรู้จัก ISO 8601, มาตรฐานรูปแบบวันที่ที่ทั่วไป นักพัฒนาจะต้องหาสมดุลระหว่างรูปแบบที่เฉพาะเจาะจงกับการตั้งค่า local ของผู้ใช้ ทำไม? เพราะวันที่มีการอ่านที่แตกต่างกันทั่วโลก เช่น ชาวอเมริกันใช้ "MM/dd/yyyy" ในขณะที่หลายประเทศในยุโรปใช้ "dd/MM/yyyy"

มีทางเลือกอื่นหรือไม่? แน่นอน Swift มี `ISO8601DateFormatter` สำหรับวันที่ ISO 8601 และ `DateComponentsFormatter` สำหรับสตริงระยะเวลา เช่น "42 นาที" คุณยังสามารถสร้างสตริงแบบกำหนดเองด้วย `.formatted()` ใน Swift เวอร์ชัน 5.5 ขึ้นไป:

```Swift
let formattedDate = date.formatted(.dateTime.year().month().day().hour().minute().second())
print(formattedDate) // ผลลัพธ์จะขึ้นอยู่กับการตั้งค่า local ของคุณ
```

ระวัง: การสร้างสตริงแบบกำหนดเองอาจนำไปสู่ปัญหาในการทำให้เป็นท้องถิ่นและโค้ดที่เสี่ยงต่อข้อผิดพลาด ให้ยึดติดกับการใช้ฟอร์แมตเตอร์และมาตรฐานเมื่อเป็นไปได้

## ดูเพิ่มเติม
- [การจัดรูปแบบวันที่](https://developer.apple.com/documentation/foundation/dateformatter) - เอกสารของ Apple เกี่ยวกับ DateFormatter
