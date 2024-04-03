---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:44.048070-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Kotlin \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E07\u0E48\u0E32\u0E22\u0E1C\u0E48\u0E32\u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19 \u0E40\u0E2B\u0E47\u0E19\u0E14\u0E49\
  \u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E27\u0E34\
  \u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49 `replace` \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E25\u0E31\u0E1A\u0E04\u0E33."
lastmod: '2024-03-17T21:57:56.159183-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\
  \u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E1C\u0E48\u0E32\u0E19\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\
  \ \u0E40\u0E2B\u0E47\u0E19\u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\
  \u0E13\u0E43\u0E0A\u0E49 `replace` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E25\u0E31\
  \u0E1A\u0E04\u0E33."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
Kotlin ทำให้การจัดการข้อความเป็นเรื่องง่ายผ่านไลบรารีมาตรฐาน เห็นด้านล่างสำหรับวิธีที่คุณใช้ `replace` เพื่อสลับคำ

```kotlin
fun main() {
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = originalText.replace("pragmatic", "cool")
    
    println(newText) // ผลลัพธ์: Kotlin is fun, Kotlin is cool!
}
```

สำหรับรูปแบบ regex:

```kotlin
fun main() {
    val regex = "Kotlin".toRegex()
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = regex.replace(originalText, "Java")
    
    println(newText) // ผลลัพธ์: Java is fun, Java is pragmatic!
}
```

## ศึกษาลึกลงไป
การเขียนข้อความใหม่เก่าแก่เท่ากับการพิมพ์ แต่ในการเขียนโปรแกรม, มันเพิ่มขึ้นกับตัวประมวลผลข้อความแรกๆ ทางเลือกอื่นๆ? แน่นอน - ฟังก์ชั่นค้นหา & แทนที่ในตัวแก้ไข, เครื่องมือบรรทัดคำสั่งเช่น `sed`. ใน Kotlin เฉพาะ, คุณมีวิธีการแบบ regex และสตริงธรรมดาที่คุณใช้ประโยชน์ได้

`replace` เป็นเรื่องง่ายสำหรับข้อความธรรมดา; `Regex` ให้คุณด้วยมีดสวิสสำหรับรูปแบบ  Regex มีความสามารถมากแต่ซับซ้อน - มันใช้ไวยากรณ์พิเศษในการจับคู่รูปแบบ คิดถึง regex เหมือนกับการเล่นวอลโด้อยู่ที่ไหน แต่คุณกำหนดกฎเรื่องที่วอลโด้ใส่

ประเด็นการใช้งานที่ต้องระวัง? จำไว้ว่า, `String` ของ Kotlin เป็นอิมมิวเทเบิล วิธีการที่เปลี่ยนแปลงข้อความจะคืนข้อความใหม่; พวกเขาไม่เปลี่ยนแปลงข้อความเดิม

## ดูเพิ่มเติม
- เอกสารการใช้งาน Kotlin เกี่ยวกับ `replace`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Regex ใน Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- `sed` แบบเดิม: https://www.gnu.org/software/sed/manual/sed.html
