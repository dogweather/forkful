---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:22.887469-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Swift, \u0E04\
  \u0E38\u0E13\u0E21\u0E35\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E19\u0E2D\u0E22\u0E39\u0E48\
  \u0E43\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `print()` \u0E0B\u0E36\
  \u0E48\u0E07\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22, \u0E21\
  \u0E31\u0E19\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\u0E2B\
  \u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E25\u0E31\u0E07\
  \u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
lastmod: '2024-03-17T21:57:56.564284-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Swift, \u0E04\u0E38\u0E13\u0E21\u0E35\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E19\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\
  \u0E31\u0E19 `print()` \u0E0B\u0E36\u0E48\u0E07\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\
  \u0E07\u0E48\u0E32\u0E22, \u0E21\u0E31\u0E19\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\
  \u0E49\u0E04\u0E38\u0E13\u0E40\u0E2B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\
  \u0E48\u0E01\u0E33\u0E25\u0E31\u0E07\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\
  \u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
ใน Swift, คุณมีเพื่อนอยู่ในฟังก์ชัน `print()` ซึ่งใช้งานง่าย, มันช่วยให้คุณเห็นสิ่งที่กำลังเกิดขึ้นในโค้ดของคุณ

```Swift
var greeting = "Hello, playground"
print(greeting)
// ผลลัพธ์: Hello, playground

let numbers = [1, 2, 3, 4, 5]
for number in numbers {
    print(number)
}
// ผลลัพธ์:
// 1
// 2
// 3
// 4
// 5
```

แต่รอซักครู่, ยังมีอีก! ต้องการข้อมูลดีบักโดยละเอียดหรือไม่? `debugPrint()` คอยให้บริการ:

```Swift
debugPrint(greeting)
// ผลลัพธ์: "Hello, playground"
```

สังเกตเห็นเครื่องหมายอัญประกาศนั้นหรือไม่? `debugPrint()` เปิดเผยข้อมูลพิเศษเกี่ยวกับประเภทข้อมูลและโครงสร้าง

## การศึกษาลึก
ในยุคของ Objective-C, เราใช้ `NSLog` เพื่อบันทึกสิ่งต่างๆ ออกมา Swift ทำให้สิ่งต่างๆ เรียบง่าย—`print()` เป็นขนมปังของคุณสำหรับผลลัพธ์มาตรฐาน, ในขณะที่ `debugPrint()` เป็นเนยรสชาติสำหรับมุมมองโดยละเอียด

ข้อเท็จจริงที่น่าสนใจ: ผลลัพธ์มาตรฐานใน Swift ไม่ได้เป็นเพียงแค่ข้อความ—มันสามารถเป็นประเภทใดๆ ที่สอดคล้องกับ `CustomStringConvertible` หรือ `CustomDebugStringConvertible` โปรโตคอลเหล่านี้ช่วยให้คุณปรับแต่งการดูของวัตถุของคุณเมื่อพวกเขาบอกเรื่องราวผ่านการพิมพ์

ภายใต้ฝาเครื่อง, `print()` และ `debugPrint()` ใช้ `String(describing:)` และ `String(reflecting:)` เพื่อเปลี่ยนวัตถุของคุณเป็นสตริง โดยพื้นฐานแล้ว, ฟังก์ชั่นเหล่านี้ใช้กระจกเพื่อถ่ายเซลฟี่ข้อมูลของคุณ

มีทางเลือกอื่นหรือไม่? คุณมี `os_log` และ `NSLog`, แต่เหล่านี้เหมาะสมกว่าสำหรับการบันทึกในระดับการผลิต, ไม่ใช่การดีบักแบบรวดเร็วและสกปรกที่เรากำลังพูดถึงที่นี่

## ดูเพิ่มเติม
- อ้างอิง API Swift ของ Apple สำหรับฟังก์ชันพิมพ์: [ไลบรารีมาตรฐาน Swift: print(_:separator:terminator:)](https://developer.apple.com/documentation/swift/1541053-print)
- การศึกษาลึกเพิ่มเติมเกี่ยวกับการบันทึกใน Swift, การพิจารณา GDPR และความเป็นส่วนตัว: [Unified Logging and Activity Tracing](https://developer.apple.com/documentation/os/logging)
- การแทรกสตริงและความสามารถในการปรับแต่งสำหรับคำอธิบายการดีบักใน Swift: [CustomStringConvertible](https://developer.apple.com/documentation/swift/customstringconvertible) และ [CustomDebugStringConvertible](https://developer.apple.com/documentation/swift/customdebugstringconvertible)
