---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:36.606734-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E2B\u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\u0E14\u0E40\u0E04\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E04\u0E33\u0E1E\u0E39\u0E14\u0E17\
  \u0E35\u0E48\u0E04\u0E23\u0E2D\u0E1A\u0E04\u0E25\u0E38\u0E21\u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32 \u0E40\u0E23\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\
  \u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\
  \u0E32,\u2026"
lastmod: '2024-03-17T21:57:56.550443-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E2B\u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\u0E14\u0E40\u0E04\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E04\u0E33\u0E1E\u0E39\u0E14\u0E17\
  \u0E35\u0E48\u0E04\u0E23\u0E2D\u0E1A\u0E04\u0E25\u0E38\u0E21\u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32 \u0E40\u0E23\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\
  \u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\
  \u0E32,\u2026"
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\
  \u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 9
---

## อะไร & ทำไม?

การลบเครื่องหมายอัญประกาศออกจากสตริงหมายถึงการกำจัดเครื่องหมายคำพูดที่ครอบคลุมเนื้อหา เราทำเช่นนี้เพื่อทำความสะอาดข้อมูลนำเข้า, เตรียมข้อมูลสำหรับการจัดเก็บ, หรือกำจัดรูปแบบข้อความที่ไม่จำเป็นซึ่งอาจจะรบกวนการประมวลผลข้อมูล

## วิธีทำ:

Swift ทำให้คุณสามารถจัดการงานลบเครื่องหมายอัญประกาศได้อย่างง่ายดาย นี่คือตัวอย่างเร็วๆ โดยใช้ `replacingOccurrences(of:with:)`, ซึ่งทำงานตามที่ฟังดู—สลับส่วนของข้อความด้วยสิ่งอื่น, หรือไม่มีอะไรเลย

```swift
var quotedString = "\"This is a 'quoted' string.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // This is a 'quoted' string.

// มีปัญหากับเครื่องหมายอัญประกาศเดี่ยวหรือไม่? เปลี่ยนคำค้นหาเท่านั้น.
quotedString = "'Here's another example.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Heres another example.
```

ผลลัพธ์จะเป็นสตริงที่ไม่มีเครื่องหมายอัญประกาศพร้อมสำหรับสิ่งที่คุณวางแผนไว้ถัดไป

## ดำดิ่งลึก

เราได้ "ทำความสะอาด" สตริงเหล่านี้มาตั้งแต่เริ่มต้นการเขียนโปรแกรม ในยุคแรก, มันเป็นเรื่องเกี่ยวกับการอนุรักษ์หน่วยความจำที่มีค่าและหลีกเลี่ยงข้อผิดพลาดของไวยากรณ์ในการประมวลผลข้อมูลนำเข้า ก้าวไปข้างหน้ามาถึงวันนี้, และมันเกี่ยวกับการรักษาความสะอาดข้อมูล—โดยเฉพาะเมื่อมีการจัดการกับ JSON หรือเตรียมสตริงสำหรับการทำงานกับฐานข้อมูล เครื่องหมายอัญประกาศที่เหลืออยู่อาจทำให้คิวรี SQL มีปัญหาได้เร็วกว่าที่คุณจะพูดว่า "ข้อผิดพลาดทางไวยากรณ์"

มีทางเลือกอื่นหรือไม่? หากคุณรู้สึก `replacingOccurrences(of:with:)` ค่อนข้างธรรมดา, คุณอาจจะสนใจในการใช้ regular expressions สำหรับรูปแบบที่ซับซ้อนกว่า หรือเมื่อคุณต้องการลบเครื่องหมายอัญประกาศเฉพาะในตำแหน่งบางตำแหน่ง เรียกใช้คลาส `NSRegularExpression` ของ Swift ได้ที่นี่ แต่จำไว้, regex อาจเป็นดาบสองคม—ทรงพลังแต่บางครั้งก็ใช้มากเกินไป

เกี่ยวกับแง่มุมการนำไปใช้, `replacingOccurrences(of:with:)` เป็นวิธีที่มีให้โดย `String` ใน Swift ซึ่งเรียกใช้ฟังก์ชันการจัดการสตริงที่ซับซ้อนมากขึ้นภายในที่จัดการกับ Unicode และข้อซับซ้อนอื่นๆ ของการประมวลผลข้อความสมัยใหม่ มันคือหนึ่งใน "ง่ายที่ผิวหน้า, ซับซ้อนภายใต้ฝาครอบ" ที่ Swift จัดการให้ คุณไม่ต้องทำ

## ดูเพิ่มเติม

สำหรับข้อมูลเพิ่มเติมเกี่ยวกับการจัดการสตริงใน Swift:

- The Swift Programming Language (Strings and Characters): [เอกสารที่ Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [เอกสารของ Apple Developer](https://developer.apple.com/documentation/foundation/nsregularexpression)

และหากคุณต้องการทราบเกี่ยวกับ regular expressions และต้องการทดสอบรูปแบบของคุณ:

- Regex101: [เครื่องมือทดสอบและตรวจสอบ Regex](https://regex101.com)
