---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:56.947866-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.579145-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีการ:


### การใช้งาน Standard Library ของ Swift
Standard Library ของ Swift มีเครื่องมือที่จำเป็นทั้งหมดสำหรับการเขียนไฟล์ข้อความ เรามาดูวิธีพื้นฐานกัน:

```swift
import Foundation

let content = "Hello, Wired readers! Learning Swift is fun."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/example.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("File written successfully")
} catch let error as NSError {
    print("Failed writing to URL: \(fileName), Error: " + error.localizedDescription)
}
```

ชิ้นส่วนโค้ดนี้เขียนสตริงลงในไฟล์ชื่อ `example.txt` ในไดเรกทอรีเอกสาร มันจัดการกับข้อผิดพลาดที่อาจเกิดขึ้นโดยใช้การจัดการข้อผิดพลาด do-try-catch ของ Swift

### การใช้ FileManager เพื่อควบคุมเพิ่มเติม
สำหรับการควบคุมคุณสมบัติของไฟล์หรือตรวจสอบว่าไฟล์มีอยู่แล้วหรือไม่ `FileManager` สามารถใช้ได้:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("example.txt")
    let content = "Exploring Swift for file management is enlightening."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("File already exists")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("File created and written successfully")
        } catch {
            print("Error writing file: \(error)")
        }
    }
}
```

### การใช้ Library ของบุคคลที่สาม
หนึ่งในไลบรารียอดนิยมของบุคคลที่สามสำหรับการดำเนินการระบบไฟล์ใน Swift คือ `Files` โดย John Sundell:

ก่อนอื่น, เพิ่ม Files เข้าไปในโปรเจกต์ของคุณ, โดยปกติผ่าน Swift Package Manager

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: ["Files"]),
    ]
)
```

จากนั้น, ใช้มันเพื่อเขียนไฟล์:

```swift
import Files

do {
    let file = try File(path: "/path/to/your/directory/example.txt")
    try file.write(string: "Swift and Files library make a powerful combination.")
    print("File written successfully using Files library.")
} catch {
    print("An error occurred: \(error)")
}
```

ด้วยไลบรารี `Files`, การจัดการไฟล์กลายเป็นเรื่องง่าย, ช่วยให้คุณสามารถโฟกัสไปที่ตรรกะของธุรกิจของแอปพลิเคชันได้มากกว่าการยุ่งยากกับการจัดการไฟล์
