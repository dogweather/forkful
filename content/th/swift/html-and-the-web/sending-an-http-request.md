---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:10.071121-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Swift \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `URLSession`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E04\u0E33\u0E02\u0E2D GET \u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22."
lastmod: '2024-03-17T21:57:56.558630-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\
  \u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E43\u0E0A\
  \u0E49\u0E04\u0E25\u0E32\u0E2A `URLSession` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E04\u0E33\u0E02\u0E2D GET \u0E17\
  \u0E35\u0E48\u0E07\u0E48\u0E32\u0E22."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
Swift ทำให้การส่งคำขอ HTTP เป็นเรื่องง่ายด้วยการใช้คลาส `URLSession` นี่คือตัวอย่างคำขอ GET ที่ง่าย:

```Swift
import Foundation

// URL ของทรัพยากรที่คุณกำลังขอ
if let url = URL(string: "https://api.example.com/data") {

    // สร้าง URLSessionDataTask
    let task = URLSession.shared.dataTask(with: url) { data, response, error in
        
        // ตรวจสอบหากมีข้อผิดพลาด
        if let error = error {
            print("Error fetching data: \(error)")
            return
        }
        
        // ตรวจสอบหากเราได้รับคำตอบที่ถูกต้องและข้อมูล
        if let httpResponse = response as? HTTPURLResponse, 
           httpResponse.statusCode == 200,
           let data = data {
            
            // แปลงข้อมูลเป็นสตริงและพิมพ์
            let dataString = String(decoding: data, as: UTF8.self)
            print(dataString)
        }
    }
    // เริ่มงาน
    task.resume()
}

// ตัวอย่างผลลัพธ์จะเป็นเนื้อหาที่ดึงมาจาก API
```

ในการส่งคำขอ POST พร้อมกับ JSON:

```Swift
import Foundation
import CoreFoundation

// จุดสิ้นสุด API ของคุณ
if let url = URL(string: "https://api.example.com/submit") {

    // เตรียมข้อมูลที่คุณต้องการส่ง
    let dictionary = ["key": "value"]
    guard let jsonData = try? JSONSerialization.data(withJSONObject: dictionary) else {
        print("Error: Cannot create JSON from dictionary")
        return
    }
    
    // เตรียม URLRequest
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    request.httpBody = jsonData
    
    // สร้างและเริ่มงาน
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        // จัดการคำตอบที่นี่
    }
    task.resume()
}

// ผลลัพธ์ขึ้นอยู่กับคำตอบจากเซิร์ฟเวอร์ ไม่มีมาตรฐานผลลัพธ์
```

## ลงลึก:
คำขอ HTTP เป็นหัวใจหลักของการสื่อสารเว็บ พวกเขามีมาตั้งแต่ยุคแรกๆ ของเว็บ ซึ่งทำให้สามารถแลกเปลี่ยนข้อมูลได้มาตรฐาน

ทางเลือกสำหรับ `URLSession` ได้แก่ ไลบรารีของบุคคลที่สามเช่น Alamofire ที่ทำให้ไวยากรณ์ง่ายขึ้นและเพิ่มความสามารถ อย่างไรก็ตาม, `URLSession` ยังคงเป็นตัวเลือกหลักสำหรับการเรียกเครือข่ายแบบเนทีฟ และ Apple ยังคงทำให้มันอัปเดตด้วยคุณสมบัติการเชื่อมต่อเครือข่ายและมาตรฐานความปลอดภัยล่าสุด

รายละเอียดการทำงานที่ควรทราบคือ คำขอเครือข่ายเป็นเรื่องของการทำงานแบบไม่พร้อมของ Swift ซึ่งทำงานอยู่ในพื้นหลัง ทำให้แอปยังคงตอบสนองได้ หากมีคำตอบกลับมา จะมีการเรียกใช้ผู้จัดการการเสร็จสิ้นงาน สิ่งสำคัญคือต้องจัดการการจัดการเธรดอย่างเหมาะสม เฉพาะเมื่ออัปเดต UI ซึ่งต้องทำบนเธรดหลัก

## ดูเพิ่มเติม:
- [URLSession | เอกสารสำหรับนักพัฒนา Apple](https://developer.apple.com/documentation/foundation/urlsession)
- [การทำงานกับ JSON ใน Swift](https://developer.apple.com/swift/blog/?id=37)
- [Alamofire GitHub Repository](https://github.com/Alamofire/Alamofire)
