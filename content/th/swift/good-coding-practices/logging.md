---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:40.004715-06:00
description: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01 (Logging) \u0E04\
  \u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\
  \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E23\u0E32\u0E22\u0E25\u0E30\
  \u0E40\u0E2D\u0E35\u0E22\u0E14\u0E1B\u0E0F\u0E34\u0E1A\u0E31\u0E15\u0E34\u0E01\u0E32\
  \u0E23, \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14 \u0E41\u0E25\
  \u0E30\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2D\u0E37\
  \u0E48\u0E19\u0E46 \u0E44\u0E1B\u0E22\u0E31\u0E07\u0E2A\u0E37\u0E48\u0E2D\u0E17\u0E35\
  \u0E48\u0E22\u0E31\u0E07\u0E04\u0E07\u0E2D\u0E22\u0E39\u0E48\u0E44\u0E21\u0E48\u0E2B\
  \u0E32\u0E22\u0E44\u0E1B \u0E40\u0E0A\u0E48\u0E19 \u0E44\u0E1F\u0E25\u0E4C\u0E2B\
  \u0E23\u0E37\u0E2D\u0E10\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
lastmod: '2024-03-17T21:57:56.567981-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01 (Logging) \u0E04\u0E37\
  \u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E23\u0E32\u0E22\u0E25\u0E30\u0E40\
  \u0E2D\u0E35\u0E22\u0E14\u0E1B\u0E0F\u0E34\u0E1A\u0E31\u0E15\u0E34\u0E01\u0E32\u0E23\
  , \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14 \u0E41\u0E25\u0E30\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2D\u0E37\u0E48\
  \u0E19\u0E46 \u0E44\u0E1B\u0E22\u0E31\u0E07\u0E2A\u0E37\u0E48\u0E2D\u0E17\u0E35\u0E48\
  \u0E22\u0E31\u0E07\u0E04\u0E07\u0E2D\u0E22\u0E39\u0E48\u0E44\u0E21\u0E48\u0E2B\u0E32\
  \u0E22\u0E44\u0E1B \u0E40\u0E0A\u0E48\u0E19 \u0E44\u0E1F\u0E25\u0E4C\u0E2B\u0E23\
  \u0E37\u0E2D\u0E10\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## อะไร & ทำไม?
การบันทึก (Logging) คือกระบวนการของการบันทึกรายละเอียดปฏิบัติการ, ข้อผิดพลาด และข้อมูลสำคัญอื่นๆ ไปยังสื่อที่ยังคงอยู่ไม่หายไป เช่น ไฟล์หรือฐานข้อมูล โปรแกรมเมอร์ทำการนี้เพื่อติดตามสถานะและประสิทธิภาพของแอปพลิเคชันของตน, การแก้ไขปัญหา และเพื่อจับตาดูสิ่งที่เกิดขึ้นภายใต้ฝากระโปรงในสภาพแวดล้อมการผลิต

## วิธีทำ:
ใน Swift คุณสามารถเขียนบันทึกไปยังคอนโซลด้วยคำสั่ง print หรือ API `os.log` ที่มีความยืดหยุ่นมากขึ้น ซึ่งเชื่อมต่อกับ Unified Logging System บนแพลตฟอร์ม Apple

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // คำสั่ง print แบบง่ายๆ
    print("Fetch started")
    
    // บันทึกระดับข้อมูลโดยใช้ os.log
    os_log(.info, log: logger, "Fetching data from API.")
    
    do {
        let data = try performNetworkRequest()
        // บันทึกระดับการดีบัก
        os_log(.debug, log: logger, "Data received: %@", data.description)
    } catch {
        // บันทึกระดับข้อผิดพลาด
        os_log(.error, log: logger, "Failed to fetch data: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // จำลองการร้องขอเชื่อมต่อเครือข่าย
    return Data()
}
```

การแสดงผลตัวอย่างบนคอนโซลอาจดูเช่นนี้:

```
Fetch started
Fetching data from API.
Data received: Some data bytes...
```

สำหรับข้อผิดพลาด อาจเป็น:

```
Failed to fetch data: The Internet connection appears to be offline.
```

## ลงลึก
การบันทึกใน Swift ได้รับพลังและประสิทธิภาพใหม่ด้วย Unified Logging System ที่เปิดตัวใน iOS 10 และ macOS Sierra ไม่เหมือนกับคำสั่ง `print` ที่ไปที่คอนโซลโดยตรง ระบบนี้เป็นกิจกรรมตามฐานะ และช่วยให้คุณกรองข้อความบันทึกตามความสำคัญและไม่ว่าพวกเขาเป็นการดีบักหรือสร้างเวอร์ชันปล่อย

บริบทประวัติศาสตร์สร้างกรอบการวิวัฒนาการของการบันทึกใน iOS และ macOS จากคำสั่ง print พื้นฐานสู่เครื่องมืออันครบถ้วนที่ทำงานร่วมกับแอพ Instruments และ Console ให้วิธีที่ซับซ้อนในการวิเคราะห์บันทึก

มีตัวเลือกอื่นๆ สำหรับการบันทึกภายใน Swift เช่น ไลบรารีของบุคคลที่สามอย่าง CocoaLumberjack ซึ่งเสนอชั้นข้อมูลมาโครเหนือ Unified Logging System มันให้การควบคุมที่เพิ่มขึ้นเกี่ยวกับการจัดรูปแบบบันทึก การจัดการไฟล์ และตัวเลือกประสิทธิภาพ

สุดท้าย รายละเอียดการนำไปใช้; OSLog ถูกออกแบบไม่เพียงแต่เพื่อประสิทธิภาพ แต่ยังสามารถรักษาข้อมูลส่วนตัวให้ปลอดภัยเมื่อบันทึก มันจัดประเภทบันทึกเป็นระดับ fault, error, info และ debug แต่ละระดับให้รายละเอียดที่แตกต่างกันสำหรับการแก้ไขปัญหา

## ดูเพิ่มเติม
- [เอกสารการบันทึกของ Apple Unified](https://developer.apple.com/documentation/os/logging)
- [บทแนะนำการบันทึกของ Ray Wenderlich ใน Swift กับ os.log](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [ที่เก็บข้อมูล GitHub ของ CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
