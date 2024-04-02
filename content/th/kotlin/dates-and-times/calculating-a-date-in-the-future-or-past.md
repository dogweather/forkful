---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:52.222331-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\
  \u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E01\u0E48\
  \u0E2D\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E23\u0E32\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E41\
  \u0E25\u0E49\u0E27 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E1F\u0E35\u0E40\u0E08\u0E2D\u0E23\u0E4C\u0E40\u0E0A\u0E48\u0E19\u0E01\
  \u0E32\u0E23\u0E40\u0E15\u0E37\u0E2D\u0E19\u0E04\u0E27\u0E32\u0E21\u0E08\u0E33,\u2026"
lastmod: '2024-03-17T21:57:56.191952-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\
  \u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E01\u0E48\
  \u0E2D\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E23\u0E32\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E41\
  \u0E25\u0E49\u0E27 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E1F\u0E35\u0E40\u0E08\u0E2D\u0E23\u0E4C\u0E40\u0E0A\u0E48\u0E19\u0E01\
  \u0E32\u0E23\u0E40\u0E15\u0E37\u0E2D\u0E19\u0E04\u0E27\u0E32\u0E21\u0E08\u0E33,\u2026"
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## อะไร & ทำไม?

การคำนวณวันที่ในอนาคตหรืออดีตหมายถึงการหาวันที่เฉพาะก่อนหรือหลังจากวันที่ทราบอยู่แล้ว โปรแกรมเมอร์ทำสิ่งนี้สำหรับฟีเจอร์เช่นการเตือนความจำ, การแจ้งเตือนวันหมดอายุ, หรือเครื่องมือวางแผน—อะไรก็ตามที่เกี่ยวข้องกับเวลา

## วิธีการ:

Kotlin จัดการวันที่และเวลาด้วยไลบรารี `java.time` ในการเพิ่มหรือลบวัน, ใช้ `plusDays()` หรือ `minusDays()` นี่คือหลักการ:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val tenDaysBefore = today.minusDays(10)
    
    println("วันนี้: $today")
    println("สิบวันจากนี้: $tenDaysLater")
    println("สิบวันที่แล้ว: $tenDaysBefore")
}
```

ผลลัพธ์ตัวอย่าง:

```
วันนี้: 2023-03-15
สิบวันจากนี้: 2023-03-25
สิบวันที่แล้ว: 2023-03-05
```

นอกเหนือจากวัน, คุณยังสามารถเล่นกับเดือนและปี (`plusMonths()`, `minusMonths()`, `plusYears()`, `minusYears()`) ได้อีกด้วย

## ทำลึก

การคำนวณวันที่ไม่ใช่เรื่องใหม่ ตั้งแต่ Java 8, แพ็กเกจ `java.time` ได้กลายเป็นการไปสู่การคำนวณเวลาวันที่—ดีกว่า `Calendar` หรือ `Date` เก่า ๆ ที่ไม่คล่องตัวและไม่ปลอดภัยกับ threads

`java.time` ใช้วัตถุที่ไม่สามารถเปลี่ยนแปลงได้, ดังนั้นคุณจึงหลีกเลี่ยงข้อผิดพลาดที่น่ารังเกียจจากการแก้ไขวันที่ของคุณโดยไม่ตั้งใจ วัตถุเช่น `LocalDate`, `LocalTime`, `LocalDateTime`, และ `ZonedDateTime` ช่วยให้คุณสามารถแสดงแง่มุมต่าง ๆ ของเวลาได้อย่างแม่นยำ

ทางเลือก? แน่นอน ก่อน `java.time`, Joda-Time เป็นอาวุธที่เลือกใช้ ระบบเก่าบางระบบยังคงใช้มันอยู่ และในโดเมน Android, ไลบรารี ThreeTenABP นำคุณสมบัติของ `java.time` กลับมาใช้ใหม่เพื่อความเข้ากันได้กับ Java 6 & 7

API `java.time` ยังถูกออกแบบให้ตระหนักถึงโซนเวลา, ด้วยคลาสเช่น `ZonedDateTime` ดังนั้นเมื่อคุณกำลังย้ายวันที่ไปรอบ ๆ, คุณสามารถเคารพต่อความเป็นมาของการหมุนของโลก

## ดูเพิ่มเติม

- เอกสารอย่างเป็นทางการของ `java.time`: [Java SE Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- สำหรับนักพัฒนา Android, รายละเอียดไลบรารี `ThreeTenABP`: [ThreeTenABP บน GitHub](https://github.com/JakeWharton/ThreeTenABP)
- คู่มือลึกล้ำ, หากคุณต้องการข้อมูลเพิ่มเติมเกี่ยวกับวันและเวลา: [Date and Time in Java](https://www.baeldung.com/java-8-date-time-intro)
