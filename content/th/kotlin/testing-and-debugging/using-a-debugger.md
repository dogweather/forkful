---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:35.445585-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E33\u0E14\u0E34\u0E48\u0E07\u0E40\u0E02\u0E49\
  \u0E32\u0E44\u0E1B\u0E43\u0E19\u0E15\u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\
  \u0E01\u0E2D\u0E23\u0E4C\u0E21\u0E35\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E14\u0E34\u0E19\u0E15\u0E32\u0E21\u0E42\u0E04\u0E49\
  \u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E08\u0E31\u0E1A\u0E15\u0E32\u0E14\u0E39\
  \u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E27\u0E48\u0E32\u0E40\u0E01\u0E35\u0E22\u0E23\
  \u0E4C\u0E01\u0E33\u0E25\u0E31\u0E07\u0E2B\u0E21\u0E38\u0E19\u0E41\u0E25\u0E30\u0E08\
  \u0E31\u0E1A\u0E1A\u0E31\u0E04\u0E17\u0E35\u0E48\u0E19\u0E48\u0E32\u0E23\u0E33\u0E04\
  \u0E32\u0E0D\u0E44\u0E14\u0E49\u0E17\u0E31\u0E19\u0E17\u0E35\u0E17\u0E35\u0E48\u0E21\
  \u0E31\u0E19\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.180722-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E33\u0E14\u0E34\u0E48\u0E07\u0E40\u0E02\u0E49\
  \u0E32\u0E44\u0E1B\u0E43\u0E19\u0E15\u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\
  \u0E01\u0E2D\u0E23\u0E4C\u0E21\u0E35\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E14\u0E34\u0E19\u0E15\u0E32\u0E21\u0E42\u0E04\u0E49\
  \u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E08\u0E31\u0E1A\u0E15\u0E32\u0E14\u0E39\
  \u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E27\u0E48\u0E32\u0E40\u0E01\u0E35\u0E22\u0E23\
  \u0E4C\u0E01\u0E33\u0E25\u0E31\u0E07\u0E2B\u0E21\u0E38\u0E19\u0E41\u0E25\u0E30\u0E08\
  \u0E31\u0E1A\u0E1A\u0E31\u0E04\u0E17\u0E35\u0E48\u0E19\u0E48\u0E32\u0E23\u0E33\u0E04\
  \u0E32\u0E0D\u0E44\u0E14\u0E49\u0E17\u0E31\u0E19\u0E17\u0E35\u0E17\u0E35\u0E48\u0E21\
  \u0E31\u0E19\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
---

## วิธีใช้:
นี่คือรสชาติเล็กน้อยของการดีบักใน Kotlin ด้วย IntelliJ IDEA - Sherlock Holmes ของ IDE:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("ทายตัวเลข: ")
        guess = readLine()?.toIntOrNull() ?: continue // ไม่สนใจอินพุตที่ไม่ดี

        // ตั้งจุดหยุดที่นี่เพื่อดู 'guess' ทำงาน
        if (guess < mysteryNumber) {
            println("น้อยเกินไป!")
        } else if (guess > mysteryNumber) {
            println("มากเกินไป!")
        }
    }

    println("คุณทายถูกต้อง! ตัวเลขลึกลับคือ $mysteryNumber")
}
```

ผลลัพธ์จากตัวดีบักเกอร์:
```
ทายตัวเลข: 
10
น้อยเกินไป!
ทายตัวเลข: 
50
มากเกินไป!
ทายตัวเลข: 
42
คุณทายถูกต้อง! ตัวเลขลึกลับคือ 42
```

## ลงลึกยิ่งขึ้น
ตัวดีบักเกอร์มีมาตั้งแต่ยุค '50s ในตอนนั้น พวกมันยังค่อนข้างเริ่มต้น และการดีบักอาจเกี่ยวข้องกับฮาร์ดแวร์มากกว่าซอฟต์แวร์ ในปัจจุบัน ตัวดีบักเกอร์เช่นตัวหนึ่งใน IntelliJ IDEA ช่วยให้เราสามารถตั้งจุดหยุด, เดินตามโค้ดแบบบรรทัดต่อบรรทัด และตรวจสอบสถานะของตัวแปรได้ตามสะดวก

ในขณะที่ตัวดีบักเกอร์ของ IntelliJ มีประโยชน์มากสำหรับ Kotlin มันไม่ใช่ตัวเลือกเดียวในทะเล มีตัวเลือกต่างๆเช่น Logcat สำหรับการพัฒนา Android หรือเครื่องมือบรรทัดคำสั่งเช่น jdb สำหรับผู้ที่ชอบความเรียบง่าย เวทมนตร์ที่อยู่ใต้ฝาครอบที่นี่ส่วนใหญ่เกี่ยวข้องกับ JVM Tool Interface (JVMTI) ซึ่งช่วยให้ตัวดีบักเกอร์สามารถโต้ตอบกับ Java Virtual Machine ช่วยให้นักพัฒนา Kotlin อยู่ในวงจร

## ดูเพิ่มเติม
- เอกสารตัวดีบักเกอร์ของ IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
