---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:13.229511-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E25\u0E2D\u0E07\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E25\u0E07\u0E04\u0E2D\
  \u0E19\u0E42\u0E0B\u0E25\u0E01\u0E31\u0E19."
lastmod: '2024-03-17T21:57:56.178500-06:00'
model: gpt-4-0125-preview
summary: "\u0E25\u0E2D\u0E07\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E25\u0E07\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
ลองพิมพ์ข้อมูลลงคอนโซลกัน:

```Kotlin
fun main() {
    val magicNumber = 42
    println("The magic number is $magicNumber")

    debugPrint("The magic number squared equals ${magicNumber * magicNumber}")
}

fun debugPrint(message: String) {
    if (BuildConfig.DEBUG) {
        println("DEBUG: $message")
    }
}
```
ผลลัพธ์ตัวอย่าง:
```
The magic number is 42
DEBUG: The magic number squared equals 1764
```
อย่างง่ายและรวดเร็ว, คุณสามารถเห็นค่าของคุณตรงนั้นๆ ในคอนโซลเลย

## ลงลึก
การพิมพ์ไปยังคอนโซลเพื่อการแก้จุดบกพร่องนั้นเก่าแก่เท่ากับภูเขา เป็นเรื่องง่าย, มีอยู่ในทุกภาษาการเขียนโปรแกรม, และสามารถทำงานได้ดี แต่มันไม่หรูหรา, และในระบบที่ซับซ้อน, ข้อมูลเอาต์พุตที่มากเกินไปอาจเป็นเรื่องยุ่งเหยิง

ทางเลือกอื่นๆ สำหรับ `println` ใน Kotlin อาจเป็นการใช้เฟรมเวิร์กการบันทึกเช่น `Log4j` หรือเครื่องมือ `Logging` ที่มีให้ภายใน Kotlin ซึ่งช่วยกรองข้อความตามระดับความรุนแรง

นัยยะหนึ่งของ Kotlin, เห็นได้จากฟังก์ชัน `debugPrint` ของเรา, คือการตรวจสอบว่าเราอยู่ในบิลด์ระดับแก้จุดบกพร่องหรือไม่; วิธีนี้ช่วยให้เราไม่เติมเต็มบันทึกการทำงานระหว่างการผลิตด้วยข้อความแก้จุดบกพร่องของเรา ช่วยเก็บการเผยแพร่จริงให้สะอาดและเป็นมิตรกับผู้ใช้

## ดูเพิ่มเติม
- สำหรับบทนำเกี่ยวกับการบันทึกข้อมูลใน Kotlin, ดูที่เอกสารอย่างเป็นทางการ: [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging)
- มุมมองของ JetBrains เกี่ยวกับกลยุทธ์การแก้จุดบกพร่อง: [IntelliJ IDEA Debugging](https://www.jetbrains.com/help/idea/debugging-code.html)
- หากคุณใช้ Android, คู่มืออย่างเป็นทางการเกี่ยวกับการใช้ Logcat มีค่าไม่แพ้กัน: [เอกสาร Android Logcat](https://developer.android.com/studio/command-line/logcat)
