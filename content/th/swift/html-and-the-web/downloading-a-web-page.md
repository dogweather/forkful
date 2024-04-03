---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:23.961394-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\
  \u0E01\u0E40\u0E27\u0E47\u0E1A\u0E41\u0E25\u0E30\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\
  \u0E21\u0E32\u0E43\u0E19\u0E41\u0E2D\u0E1B\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E19\
  \u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E35\u0E22\u0E01\u0E14\u0E39\
  \u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32, \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\
  \u0E01\u0E31\u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E2D\u0E2D\u0E19\u0E44\u0E25\
  \u0E19\u0E4C, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25."
lastmod: '2024-03-17T21:57:56.560539-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\
  \u0E01\u0E40\u0E27\u0E47\u0E1A\u0E41\u0E25\u0E30\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\
  \u0E21\u0E32\u0E43\u0E19\u0E41\u0E2D\u0E1B\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E19\
  \u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E35\u0E22\u0E01\u0E14\u0E39\
  \u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32, \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\
  \u0E01\u0E31\u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E2D\u0E2D\u0E19\u0E44\u0E25\
  \u0E19\u0E4C, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25."
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## วิธีการ:
ลองใช้ `URLSession` เพื่อทำงานนี้ Swift ทำให้มันตรงไปตรงมา

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Error:", error)
        return
    }

    if let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) {
        if let mimeType = httpResponse.mimeType, mimeType == "text/html",
           let data = data, let string = String(data: data, encoding: .utf8) {
            print("เนื้อหาเว็บเพจที่ดาวน์โหลดมา:")
            print(string)
        } else {
            print("ประเภท MIME หรือการเข้ารหัสไม่ถูกต้อง.")
        }
    } else {
        print("เซิร์ฟเวอร์ตอบกลับด้วยข้อผิดพลาด.")
    }
}
task.resume()
// ตรวจสอบให้แน่ใจว่า playground ทำงานต่อเนื่องจนกว่างานจะเสร็จสิ้น
RunLoop.current.run()
```

ผลลัพธ์ตัวอย่างอาจดูเช่นนี้:

```
เนื้อหาเว็บเพจที่ดาวน์โหลดมา:
<!doctype html>...
```

## ลงลึก
API `URLSession` ได้รับการเปิดตัวตั้งแต่ iOS 7 และ macOS 10.9 มันเป็นการเปลี่ยนแปลงที่สำคัญในขณะนั้น, ได้แทนที่ `NSURLConnection` ที่เก่ากว่าและยุ่งยากกว่า ในขณะที่ `URLSession` ทรงพลังและยืดหยุ่น, คุณอาจพิจารณาไลบรารีของบุคคลที่สามเช่น Alamofire สำหรับความต้องการเครือข่ายที่ซับซ้อนมากขึ้น

เมื่อดำเนินการ, จำไว้ว่าคำขอเครือข่ายเป็นแบบไม่ต้องรอการตอบสนอง ซึ่งหมายความว่าแอปของคุณสามารถดำเนินการอื่น ๆ ได้ในขณะที่เซิร์ฟเวอร์ตอบกลับมาหาคุณ นอกจากนี้, การใช้ `URLSession` อย่างเหมาะสมประกอบด้วยการจัดการข้อผิดพลาดอย่างมีเมตตาและตรวจสอบสถานะตอบกลับจากเซิร์ฟเวอร์ การตรวจสอบประเภท MIME เป็นสิ่งสำคัญเพื่อให้แน่ใจว่าคุณได้รับ HTML, ไม่ใช่ประเภทไฟล์อื่นๆ เช่น JSON หรือภาพ

## ดูเพิ่มเติม
ลงลึกเพิ่มเติมหรือสำรวจทางเลือก:
- เอกสาร `URLSession` ของ Apple: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Swift networking กับ Alamofire: [Alamofire](https://github.com/Alamofire/Alamofire)
- แพทเทิร์น async/await ของ Swift สำหรับ `URLSession` ใน iOS 15+: [URLSession async/await](https://developer.apple.com/videos/play/wwdc2021/10054/)
