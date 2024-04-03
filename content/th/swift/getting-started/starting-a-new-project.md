---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:37.145307-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: ."
lastmod: '2024-03-17T21:57:56.562487-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
weight: 1
---

## วิธีทำ:
```Swift
import SwiftUI

@main
struct NewProjectApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

struct ContentView: View {
    var body: some View {
        Text("สวัสดี, โปรเจคใหม่!")
            .padding()
    }
}

// ตัวอย่างผลลัพธ์:
// จะแสดงหน้าต่างพร้อมข้อความ "สวัสดี, โปรเจคใหม่!"
```

## ลงลึก
ในยุคก่อนหน้าภาษา Swift, Objective-C เป็นภาษาหลักและการเริ่มต้นโปรเจคใหม่ต้องใช้โค้ดพื้นฐานเพิ่มเติม อย่างไรก็ตาม Swift ได้พัฒนากระบวนการเริ่มต้นด้วยคุณสมบัตินี้สนุก ๆ  เช่น องค์ประกอบ `@main` ที่ระบุจุดเริ่มต้นของแอป หากเปรียบเทียบกับเครื่องมือต่าง ๆ เช่น แม่แบบของ Xcode, Swift ทำให้งานที่น่าเบื่อเรียบง่ายขึ้นเพื่อว่าคุณสามารถกระโดดไปยังส่วนสนุก – ทำให้ไอเดียของคุณมีชีวิตขึ้นมา

สำหรับทางเลือกอื่น ๆ คุณอาจใช้เครื่องมือบรรทัดคำสั่งหรือเฟรมเวิร์กฝั่งเซิร์ฟเวอร์ถ้าคุณไม่ทำแอป iOS/macOS ในแง่ของการปฏิบัติ Swift มีวัตถุประสงค์ในการลดความซับซ้อนเริ่มต้น `ContentView` แทนจุดเริ่มต้นของ UI ในขณะที่ `WindowGroup` จัดการกับการจัดการหน้าต่าง

## ดูเพิ่มเติม
- [เอกสาร Swift](https://swift.org/documentation/)
- [บทช่วยสอน SwiftUI ของ Apple](https://developer.apple.com/tutorials/swiftui)
- [เริ่มพัฒนาแอป iOS (Swift)](https://developer.apple.com/library/archive/referencelibrary/GettingStarted/DevelopiOSAppsSwift/)
