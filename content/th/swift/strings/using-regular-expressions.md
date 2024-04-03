---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:24.701310-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E2A\
  \u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E40\u0E19\u0E17\u0E35\u0E1F\u0E02\u0E2D\
  \u0E07 Swift \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A regex \u0E43\u0E0A\u0E49\u0E04\
  \u0E25\u0E32\u0E2A `NSRegularExpression` \u0E04\u0E27\u0E1A\u0E04\u0E39\u0E48\u0E44\
  \u0E1B\u0E01\u0E31\u0E1A\u0E40\u0E21\u0E17\u0E47\u0E2D\u0E14 range \u0E41\u0E25\u0E30\
  \ replacement \u0E02\u0E2D\u0E07\u0E04\u0E25\u0E32\u0E2A String\u2026"
lastmod: '2024-03-17T21:57:56.552281-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E40\
  \u0E19\u0E17\u0E35\u0E1F\u0E02\u0E2D\u0E07 Swift \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \ regex \u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `NSRegularExpression` \u0E04\u0E27\
  \u0E1A\u0E04\u0E39\u0E48\u0E44\u0E1B\u0E01\u0E31\u0E1A\u0E40\u0E21\u0E17\u0E47\u0E2D\
  \u0E14 range \u0E41\u0E25\u0E30 replacement \u0E02\u0E2D\u0E07\u0E04\u0E25\u0E32\
  \u0E2A String \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E01\u0E32\
  \u0E23\u0E43\u0E0A\u0E49 regex \u0E43\u0E19\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\
  \u0E32\u0E41\u0E25\u0E30\u0E40\u0E19\u0E49\u0E19\u0E17\u0E35\u0E48\u0E2D\u0E22\u0E39\
  \u0E48 email \u0E20\u0E32\u0E22\u0E43\u0E19\u0E1A\u0E25\u0E47\u0E2D\u0E01\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## วิธีการ:
การสนับสนุนเนทีฟของ Swift สำหรับ regex ใช้คลาส `NSRegularExpression` ควบคู่ไปกับเมท็อด range และ replacement ของคลาส String ด้านล่างเป็นตัวอย่างของการใช้ regex ในการค้นหาและเน้นที่อยู่ email ภายในบล็อกข้อความ:

```swift
import Foundation

let text = "ติดต่อเราที่ support@example.com หรือ feedback@example.org เพื่อขอข้อมูลเพิ่มเติม."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("พบ: \(text[range])")
        }
    } else {
        print("ไม่พบการจับคู่.")
    }
} catch {
    print("ข้อผิดพลาด Regex: \(error.localizedDescription)")
}

// ตัวอย่างผลลัพธ์:
// พบ: support@example.com
// พบ: feedback@example.org
```

สำหรับสถานการณ์ที่ซับซ้อนหรือมุ่งเน้นความสะดวกมากขึ้น คุณสามารถใช้ไลบรารีของบุคคลที่สาม เช่น SwiftRegex ซึ่งทำให้ไวยากรณ์เรียบง่ายและขยายความเป็นไปได้ แม้ว่าไลบรารีมาตรฐานของ Swift จะทรงพลัง แต่บางนักพัฒนาก็ชอบไลบรารีเหล่านี้เพราะไวยากรณ์ที่กระชับและคุณสมบัติเพิ่มเติม นี่คือวิธีที่คุณอาจดำเนินการงานที่คล้ายคลึงกันโดยใช้ไลบรารีบุคคลที่สามที่สมมุติขึ้น:

```swift
// สมมติว่ามีไลบรารีชื่อ SwiftRegex และมีการนำเข้าที่ใช้
let text = "ติดต่อได้ที่ hello@world.com หรือเยี่ยมชมเว็บไซต์ของเรา."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // วิธีการสมมติที่ SwiftRegex มีให้
if emails.isEmpty {
    print("ไม่พบที่อยู่อีเมล.")
} else {
    emails.forEach { email in
        print("พบ: \(email)")
    }
}

// ผลลัพธ์สมมติสมมติว่าวิธีการ `matches(for:)` มีอยู่ใน SwiftRegex:
// พบ: hello@world.com
```

ตัวอย่างนี้แสดงการใช้แพ็กเกจ regular expression ของบุคคลที่สามเพื่อเรียบง่ายการค้นหาการจับคู่ภายในสตริง โดยสันนิษฐานว่ามีวิธีการสะดวกเช่น `matches(for:)` สำคัญที่ต้องอ้างอิงเอกสารของไลบรารีบุคคลที่สามเพื่อความถูกต้องของไวยากรณ์และความพร้อมใช้งานของวิธีการ.
