---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:11.631348-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Swift \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E2A\
  \u0E48\u0E27\u0E19\u0E22\u0E48\u0E2D\u0E22\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\u0E48\
  \u0E2D\u0E19\u0E02\u0E49\u0E32\u0E07\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\
  \u0E21\u0E32 \u0E21\u0E32\u0E25\u0E2D\u0E07\u0E14\u0E33\u0E14\u0E34\u0E48\u0E07\u0E25\
  \u0E07\u0E44\u0E1B\u0E14\u0E39\u0E01\u0E31\u0E19\u0E40\u0E25\u0E22\u0E14\u0E49\u0E27\
  \u0E22\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07."
lastmod: '2024-03-17T21:57:56.551372-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E01\u0E31\u0E1A\u0E2A\u0E48\u0E27\u0E19\u0E22\u0E48\u0E2D\u0E22\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E04\u0E48\u0E2D\u0E19\u0E02\u0E49\u0E32\u0E07\u0E15\u0E23\
  \u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32 \u0E21\u0E32\u0E25\u0E2D\u0E07\u0E14\
  \u0E33\u0E14\u0E34\u0E48\u0E07\u0E25\u0E07\u0E44\u0E1B\u0E14\u0E39\u0E01\u0E31\u0E19\
  \u0E40\u0E25\u0E22\u0E14\u0E49\u0E27\u0E22\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## วิธีการ:
Swift ทำให้การทำงานกับส่วนย่อยสตริงค่อนข้างตรงไปตรงมา มาลองดำดิ่งลงไปดูกันเลยด้วยตัวอย่าง

```swift
let fullString = "Hello, Swift Programmer!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 12)

// การแยกส่วนย่อยสตริงโดยใช้ String.Index
let substring = fullString[startIndex...endIndex]

print(substring) // "Swift"

// อีกวิธีหนึ่ง, โดยใช้ NSRange และ NSString
import Foundation

let nsRange = NSRange(location: 7, length: 6)
if let range = Range(nsRange, in: fullString) {
    let substring = fullString[range]
    print(substring) // "Swift"
}

// วิธีสั้น, ถ้าคุณรู้ดัชนีที่แน่นอน
let quickSubstring = fullString[7...12]

print(quickSubstring) // จะเกิดข้อผิดพลาดเพราะ Swift strings ไม่รองรับการดัชนีด้วยเลขจำนวนเต็ม
```

ผลลัพธ์:
```
Swift
Swift
// ข้อผิดพลาด: 'subscript(_:)' ไม่สามารถใช้งานได้: ไม่สามารถใช้ subscript String ด้วย Int ได้ ดูเอกสารของ String เพื่อข้อมูลเพิ่มเติม
```

## การศึกษาลึกลงไป
การแยกส่วนย่อยสตริงใน Swift ต้องเข้าใจว่า Swift จัดการกับสตริงอย่างไร ซึ่งแตกต่างจากภาษาเช่น Python หรือ C# ใน Swift, สตริงเป็นคอลเล็กชันของอักขระที่ไม่ใช้ดัชนีจำนวนเต็ม สิ่งนี้มาจากการที่ Swift รองรับอักขระที่เป็นมาตรฐาน Unicode ทำให้สตริงไม่มีความยาวคงที่ แต่เป็นคอลเล็กชันของกลุ่มกราฟีม (ที่ผู้ใช้รับรู้เป็นอักขระเดียว)

การออกแบบนี้หมายความว่าการดัชนีโดยตรงด้วยเลขจำนวนเต็มไม่เหมาะกับสตริง Swift; คุณต้องทำงานด้วย `String.Index` แม้ว่าจะไม่ใช่สิ่งที่ใช้งานง่ายเช่นการใช้เลขจำนวนเต็ม แต่มันก็จัดการกับสคริปต์ข้อความและอิโมจิได้อย่างสอดคล้องกัน

ทางเลือกอื่น ๆ รวมถึงการใช้ `NSString` จาก Objective-C ตามตัวอย่างที่แสดง ซึ่งอนุญาตให้ใช้ NSRange แต่นั่นเป็นสิ่งที่ค่อนข้างเก่าและไม่เป็น Swift นับตั้งแต่ Swift 4 สตริงเองได้รับการปรับปรุงมากมายด้วย API ที่มีประสิทธิภาพและใช้งานง่ายมากขึ้นเพื่อทำงานกับส่วนย่อยสตริง ทำให้ `NSString` ไม่ค่อยถูกใช้สำหรับงานส่วนใหญ่

รายละเอียดการปฏิบัติมีความสำคัญ—การแยกส่วนย่อยสตริงอย่างง่ายๆ อาจทำให้เกิดผลกระทบต่อประสิทธิภาพเนื่องจากการเรียกใช้ `index(_: offsetBy:)` อาจเป็น O(n) เมื่อจัดการกับสตริงที่รองรับ Unicode นอกจากนี้ เมื่อคุณสร้างส่วนย่อยสตริงใน Swift มันจะแชร์หน่วยความจำของสตริงต้นฉบับทำให้มีประสิทธิภาพ แต่ต้องระวังหากคุณแก้ไขสตริงต้นฉบับในภายหลัง

## ดูเพิ่มเติม
สำหรับหัวข้อนี้เพิ่มเติม สามารถเข้าชมเอกสารอย่างเป็นทางการ:

- Swift String และ Characters: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- คู่มือการเขียนโปรแกรมสตริง: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/)

ใช้เวลาฝึกฝนและเล่นใน Swift playground เพื่อทำความเข้าใจมันอย่างลึกซึ้ง
