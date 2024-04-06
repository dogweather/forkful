---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:38.075519-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19\u0E42\u0E04\
  \u0E49\u0E14\u0E02\u0E49\u0E32\u0E07\u0E15\u0E49\u0E19, `args` \u0E40\u0E1B\u0E47\
  \u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\
  \u0E47\u0E1A\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\
  \u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 \u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `main` \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E27\u0E48\u0E32\u0E40\u0E23\u0E32\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E2D\
  \u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E2B\u0E23\u0E37\
  \u0E2D\u0E44\u0E21\u0E48 \u0E41\u0E25\u0E30\u0E17\u0E31\u0E01\u0E17\u0E32\u0E22\u0E15\
  \u0E32\u0E21\u0E19\u0E31\u0E49\u0E19."
lastmod: '2024-04-05T21:54:01.852161-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E49\u0E32\u0E07\u0E15\u0E49\
  \u0E19, `args` \u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\
  \u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\
  \u0E2A\u0E31\u0E48\u0E07 \u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `main`\
  \ \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E40\u0E23\u0E32\
  \u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\
  \u0E21\u0E19\u0E15\u0E4C\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E41\u0E25\u0E30\
  \u0E17\u0E31\u0E01\u0E17\u0E32\u0E22\u0E15\u0E32\u0E21\u0E19\u0E31\u0E49\u0E19."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## วิธีการ:
```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hello, ${args[0]}!")
    } else {
        println("Hello, unknown person!")
    }
}

// ตัวอย่างผลลัพธ์ถ้าใส่ 'Kotlinista' เป็นอาร์กิวเมนต์:
// Hello, Kotlinista!
```

ในโค้ดข้างต้น, `args` เป็นอาร์เรย์ที่เก็บอาร์กิวเมนต์บรรทัดคำสั่ง ฟังก์ชัน `main` ตรวจสอบว่าเราได้รับอาร์กิวเมนต์หรือไม่ และทักทายตามนั้น

## ลงลึก
ความคิดเรื่องอาร์กิวเมนต์บรรทัดคำสั่งเก่าแก่เหมือนเนินเขา; มันเป็นส่วนหนึ่งของการเขียนโปรแกรมตั้งแต่เริ่มแรกของยุค—หรืออย่างน้อยตั้งแต่การสร้างแทอมินัลโต้ตอบ ในบริบทของ Kotlin, ซึ่งทำงานบน JVM, อาร์กิวเมนต์บรรทัดคำสั่งทำงานคล้ายกับ Java

ภาษาอื่นๆ ก็เสนอวิธีการที่คล้ายคลึงกัน เช่น `argv` ใน Python หรือ `$argc` และ `$argv` ใน PHP การเข้าหาของ Kotlin ทำให้มันง่าย—ฟังก์ชัน `main` เพียงแค่ใช้ `Array<String>`

สำหรับรายละเอียดการนำไปใช้งาน จำไว้ว่าดัชนีอาร์เรย์เริ่มต้นที่ศูนย์ `args[0]` เป็นอาร์กิวเมนต์แรก, `args[1]` เป็นอาร์กิวเมนต์ที่สอง, และต่อไปเรื่อยๆ นอกจากนี้ คำนึงไว้ด้วยว่าหากคุณกำลังพัฒนาแอปที่ซับซ้อนซึ่งต้องการแยกวิเคราะห์คำสั่งได้อย่างยืดหยุ่นมากขึ้น คุณอาจต้องการพิจารณาใช้ไลบรารีเฉพาะทางเช่น kotlinx-cli

## ดูเพิ่มเติม
- [เอกสารอย่างเป็นทางการของ Kotlin เกี่ยวกับการใช้งานแอปพลิเคชันบรรทัดคำสั่ง](https://kotlinlang.org/docs/command-line.html)
- [kotlinx-cli บน GitHub](https://github.com/Kotlin/kotlinx-cli)
