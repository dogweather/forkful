---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:18.133556-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Swift \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19\u0E1A\u0E23\u0E23\u0E17\u0E31\
  \u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\
  \u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E21\u0E32\u0E01 \u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E44\u0E14\u0E49\u0E1C\u0E48\
  \u0E32\u0E19\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07 `CommandLine`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E2A\u0E32\u0E23\u0E30\u0E2A\u0E33\u0E04\
  \u0E31\u0E0D."
lastmod: '2024-03-17T21:57:56.576352-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\
  \u0E19\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\
  \u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E21\
  \u0E32\u0E01 \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\
  \u0E07\u0E44\u0E14\u0E49\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07 `CommandLine` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E2A\u0E32\
  \u0E23\u0E30\u0E2A\u0E33\u0E04\u0E31\u0E0D."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## วิธีการ:
Swift ทำให้การอ่านอาร์กิวเมนต์บนบรรทัดคำสั่งเป็นเรื่องง่ายมาก สามารถเข้าถึงได้ผ่านโครงสร้าง `CommandLine` นี่คือสาระสำคัญ:

```swift
for argument in CommandLine.arguments {
    print(argument)
}
```

หากคุณใส่โค้ดนี้เข้าไปในไฟล์ `main.swift` และรันโปรแกรมของคุณพร้อมกับข้อความเพิ่มเติม เช่น `swift run YourProgram foo bar`, ผลลัพธ์ของคุณจะดูเป็นแบบนี้:

```
/path/to/YourProgram
foo
bar
```

นั่นหมายถึงการพิมพ์อาร์กิวเมนต์ทุกรายการออกมา รวมถึงทางที่อยู่ของโปรแกรมของคุณเป็นองค์ประกอบแรก – ต้องจำเสมอไว้!

## การศึกษาลึก
ในอดีต อาร์กิวเมนต์บนบรรทัดคำสั่งได้เป็นส่วนสำคัญในการเขียนโปรแกรม ช่วยให้ผู้คนสามารถปรับแต่งพฤติกรรมของโปรแกรมโดยไม่ต้องเปลี่ยนแปลงโค้ด มันคือมรดกของ Unix และเกือบทุกภาษาสนับสนุนฟีเจอร์นี้

ใน Swift, `CommandLine.arguments` เป็นอาร์เรย์ของสตริงที่มีแต่ละองค์ประกอบเป็นส่วนย่อยของข้อมูลนำเข้าของคุณ แยกโดยช่องว่าง อาร์เรย์นี้ถูกส่งมอบโดยระบบปฏิบัติการเมื่อโปรแกรมของคุณเริ่มทำงาน; Swift ทำให้มันง่ายต่อการเข้าถึง

นอกจาก `CommandLine.arguments`, คุณอาจจะได้พบกับการแยกวิเคราะห์ที่ซับซ้อนยิ่งขึ้นกับไลบรารี่เช่น `Swift Argument Parser` สำหรับงานที่หนักกว่านี้ นี่คือสิ่งที่มีประโยชน์เมื่อคุณต้องการสิ่งที่เกินกว่าแค่อินพุตง่ายๆ – คิดถึงธง, ตัวเลือก และคำสั่งย่อย

ในเชิงการดำเนินการ,อาร์กิวเมนต์บนบรรทัดคำสั่งเหล่านั้นมาถึงคุณผ่านอาร์เรย์ C ในเบื้องหลัง – อาร์กิวเมนต์จำนวน (`argc`) และอาร์กิวเมนต์ศูนย์อาร์กิว (`argv`) ในภาษาดี Swift ซ่อนอาร์เรย์ C แต่ยังคงรักษาพฤติกรรมพื้นฐานเดียวกันที่คุณจะพบใน C หรือ C++

## ดูเพิ่มเติม
- สำหรับมุมมองที่กว้างขวางเกี่ยวกับโปรแกรมบนบรรทัดคำสั่งใน Swift, ตรวจสอบที่ [เอกสาร Swift.org](https://swift.org/getting-started/#using-the-package-manager).
- หากต้องการประสิทธิภาพการวิเคราะห์อาร์กิวเมนต์ของคุณ, ไปดูที่ [รีพอสิทอรี GitHub ของ Swift Argument Parser](https://github.com/apple/swift-argument-parser) สำหรับการตั้งค่าที่ซับซ้อนมากขึ้น
- หากคุณสนใจว่าภาษาอื่นจัดการกับเรื่องนี้อย่างไร, ลองเปรียบเทียบกับ `sys.argv` ของ Python หรือ `process.argv` ของ Node
