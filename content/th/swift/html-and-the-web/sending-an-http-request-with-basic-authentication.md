---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:21.166504-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\
  \u0E02\u0E2D HTTP \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\
  \u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E1E\u0E37\u0E49\u0E19\u0E10\
  \u0E32\u0E19\u0E43\u0E19 Swift."
lastmod: '2024-03-17T21:57:56.561478-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\u0E23\u0E49\u0E2D\u0E21\
  \u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\
  \u0E34\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E43\u0E19 Swift."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
นี่คือวิธีการส่งคำขอ HTTP พร้อมการตรวจสอบสิทธิพื้นฐานใน Swift:

```Swift
import Foundation

// จุดสิ้นสุดของ API ของคุณ
let url = URL(string: "https://example.com/api/data")!

// ข้อมูลประจำตัวของคุณ
let username = "user"
let password = "password"

// สร้างข้อมูลการเข้าสู่ระบบและแปลงเป็นสตริง base64
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

// สร้างคำขอ
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

// ส่งคำขอ
let session = URLSession.shared
let dataTask = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)") // จัดการกับข้อผิดพลาด
    } else if let data = data, let string = String(data: data, encoding: .utf8) {
        print("Response: \(string)") // จัดการกับคำตอบ
    }
}

dataTask.resume()
```

ผลลัพธ์ควรเป็นข้อมูลที่ส่งกลับมาจาก API หรือข้อความแสดงข้อผิดพลาดถ้ามีบางอย่างผิดพลาด

## การศึกษาเชิงลึก
ในช่วงต้นของยุคเว็บ, การตรวจสอบสิทธิพื้นฐานเป็นวิธีที่รวดเร็วในการป้องกันทรัพยากร ความง่ายดายของมันทำให้ได้รับการยอมรับแพร่หลายแม้ว่าจะมีความปลอดภัยน้อยกว่าทางเลือกสมัยใหม่เช่น OAuth เนื่องจากข้อมูลประจำตัวไม่ได้เข้ารหัสแต่เพียงแค่เข้ารหัส

นอกเหนือจาก authentication พื้นฐาน, ทางเลือกอื่น ๆ ได้แก่ authentication แบบ digest, คีย์ API, OAuth, หรือ JWT (JSON Web Tokens) แต่ละอย่างมีข้อดีและข้อเสียเกี่ยวกับความปลอดภัย, ความง่ายในการใช้งาน, และระดับของการป้องกันที่นำเสนอ

เมื่อส่งคำขอ HTTP พร้อมการตรวจสอบสิทธิพื้นฐาน, เป็นปฏิบัติที่ดีที่สุดในการให้แน่ใจว่าคุณกำลังใช้ HTTPS, เพื่อให้ข้อมูลประจำตัวที่เข้ารหัสได้รับการส่งผ่านอย่างปลอดภัย นอกจากนี้, หลีกเลี่ยงการเข้ารหัสข้อมูลประจำตัวล่วงหน้า; แทนที่ด้วยการใช้ตัวแปรสภาพแวดล้อมหรือตู้นิรภัย

## ดูเพิ่มเติม
- [Apple's URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Basic Auth RFC](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
