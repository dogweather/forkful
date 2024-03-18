---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:12.674831-06:00
description: "TOML (Tom's Obvious, Minimal Language) \u0E40\u0E1B\u0E47\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E1B\u0E47\
  \u0E19\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\
  \u0E14\u0E49\u0E27\u0E22\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E17\u0E35\
  \u0E48\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E19\u0E31\u0E01\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 TOML\u2026"
lastmod: '2024-03-17T21:57:56.583828-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E1B\u0E47\u0E19\
  \u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E14\
  \u0E49\u0E27\u0E22\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E17\u0E35\u0E48\
  \u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E19\u0E31\u0E01\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 TOML\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
---

{{< edit_this_page >}}

## อะไรและทำไม?
TOML (Tom's Obvious, Minimal Language) เป็นรูปแบบสำหรับการทำให้ข้อมูลเป็นซีเรียลที่สามารถอ่านได้ง่ายด้วยความหมายที่ชัดเจน นักโปรแกรมเมอร์ใช้ TOML สำหรับไฟล์การตั้งค่าที่ความสามารถในการอ่านโดยมนุษย์และการทำให้เป็นซีเรียลโดยเครื่องจักรได้อย่างง่ายดายเป็นสิ่งสำคัญ

## วิธีการ:
เริ่มแรก, คุณต้องมี TOML parser Swift ไม่มีฟังก์ชันนี้เข้ามาในตัว, ดังนั้นเราจะใช้ `TOMLDecoder` ติดตั้งผ่าน Swift Package Manager จากนั้นคุณสามารถทำให้ข้อมูล TOML เป็นซีเรียลและกลับเข้ารูปได้อย่างง่ายดาย

```Swift
import TOMLDecoder

let tomlString = """
title = "ตัวอย่าง TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Title: \(config.title), ผู้ถือกรรมสิทธิ์: \(config.owner.name), DOB: \(config.owner.dob)")
    } catch {
        print("พบข้อผิดพลาดในการแยกวิเคราะห์ TOML: \(error)")
    }
}
```

โค้ดนี้จะแสดงผลลัพธ์ออกมาว่า:
```
Title: ตัวอย่าง TOML, ผู้ถือกรรมสิทธิ์: Tom Preston-Werner, DOB: 1979-05-27 07:32:00 +0000
```

## ลงลึก
TOML ถูกออกแบบโดย Tom Preston-Werner, ผู้ร่วมก่อตั้ง GitHub, สำหรับการเป็นทางเลือกที่มีความเข้าใจง่ายสำหรับมนุษย์มากกว่ารูปแบบเช่น JSON หรือ YAML มีเป้าหมายเพื่อความชัดเจน, ลดโอกาสของการตีความผิดโดยมนุษย์หรือเครื่องจักร เมื่อเปรียบเทียบกับทางเลือกอื่น, YAML และ JSON เป็นตัวเลือกที่โดดเด่น, โดยที่ YAML มุ่งเน้นไปที่ความสามารถในการอ่านของมนุษย์และ JSON เป็นตัวเลือกที่ง่ายกว่าสำหรับเครื่องจักร เมื่อทำงานกับ TOML ใน Swift, เราไม่มี parser เนทีฟ อย่างไรก็ตาม, ไลบรารีบุคคลที่สามเช่น `TOMLDecoder` ช่วยให้การเปลี่ยนระหว่างสตริง TOML และประเภทใน Swift ได้ง่าย, โดยเฉพาะผ่าน `Codable` protocols ที่ถูกนำมาใช้ใน Swift 4 ซึ่งทำให้การทำซีเรียลง่ายขึ้น

## ดูเพิ่มเติม
- มาตรฐาน TOML: https://toml.io
- GitHub สำหรับ `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- เอกสารของ Swift เกี่ยวกับ `Codable`: https://developer.apple.com/documentation/swift/codable
- เปรียบเทียบรูปแบบการทำซีเรียลข้อมูล: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
