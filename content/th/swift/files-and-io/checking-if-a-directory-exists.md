---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:43.641520-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E43\u0E19\u0E23\u0E30\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E2B\u0E23\
  \u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E16\u0E37\u0E2D\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\
  \u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E08\u0E32\u0E01\u0E20\u0E32\u0E22\u0E43\
  \u0E19\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19 Swift \u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13\u2026"
lastmod: '2024-03-17T21:57:56.575405-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E43\u0E19\u0E23\u0E30\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E2B\u0E23\
  \u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E16\u0E37\u0E2D\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\
  \u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E08\u0E32\u0E01\u0E20\u0E32\u0E22\u0E43\
  \u0E19\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19 Swift \u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13\u2026"
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การตรวจสอบว่ามีไดเรกทอรีอยู่ในระบบไฟล์หรือไม่ ถือเป็นสิ่งสำคัญสำหรับการจัดการโครงสร้างไฟล์จากภายในแอปพลิเคชัน Swift ของคุณ งานนี้ช่วยให้นักพัฒนาสามารถตรวจสอบการมีอยู่ของไดเรกทอรีก่อนที่จะพยายามอ่านหรือเขียนไปยังมัน จึงช่วยหลีกเลี่ยงข้อผิดพลาดในขณะทำงานได้

## วิธีทำ:

โครงสร้างพื้นฐานของ Swift ให้คลาส `FileManager` ซึ่งมีเมธอดในการจัดการระบบไฟล์ คุณสามารถใช้ `FileManager` เพื่อตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ นี่คือตัวอย่างโค้ดที่ทำแบบนี้:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

อย่างไรก็ตาม สิ่งนี้ตรวจสอบทั้งไฟล์และไดเรกทอรี หากคุณต้องการตรวจสอบเฉพาะว่ามีไดเรกทอรีอยู่จริง คุณจำเป็นต้องส่งตัวชี้ค่าบูลีนใน `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

### การใช้งานไลบรารีจากบุคคลที่สาม

ณ เวลานี้ การตรวจสอบการมีอยู่ของไดเรกทอรีใน Swift โดยทั่วไปไม่ต้องการไลบรารีจากบุคคลที่สาม เนื่องจากคลาส `FileManager` นั้นมีประสิทธิภาพ อย่างไรก็ตาม สำหรับการจัดการไฟล์และตรวจสอบที่ซับซ้อนมากขึ้น ไลบรารีเช่น **Files** โดย John Sundell นั้นให้ API ที่เป็นมิตรกับ Swift มากขึ้น

นี่คือวิธีที่คุณอาจใช้:

ก่อนอื่น เพิ่ม Files เข้าไปในโปรเจกต์ของคุณผ่าน Swift Package Manager

จากนั้น คุณสามารถตรวจสอบการมีอยู่ของไดเรกทอรีได้ดังนี้:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Directory exists")
} catch {
    print("Directory does not exist")
}
```

หมายเหตุ: เนื่องจากไลบรารีจากบุคคลที่สามสามารถเปลี่ยนแปลงได้ จึงควรอ้างอิงเอกสารล่าสุดสำหรับการใช้งานและแนวปฏิบัติที่ดีที่สุดเสมอ
