---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:20.545809-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\
  \u0E14\u0E49\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\
  \u0E1E\u0E25\u0E32\u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\
  \u0E08\u0E09\u0E31\u0E22\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E01\u0E23\u0E30\u0E41\u0E2A\
  \ (stream) \u0E40\u0E09\u0E1E\u0E32\u0E30\u0E17\u0E35\u0E48\u0E41\u0E22\u0E01\u0E15\
  \u0E48\u0E32\u0E07\u0E2B\u0E32\u0E01\u0E08\u0E32\u0E01\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u2026"
lastmod: '2024-03-17T21:57:56.196162-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\
  \u0E14\u0E49\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\
  \u0E1E\u0E25\u0E32\u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\
  \u0E08\u0E09\u0E31\u0E22\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E01\u0E23\u0E30\u0E41\u0E2A\
  \ (stream) \u0E40\u0E09\u0E1E\u0E32\u0E30\u0E17\u0E35\u0E48\u0E41\u0E22\u0E01\u0E15\
  \u0E48\u0E32\u0E07\u0E2B\u0E32\u0E01\u0E08\u0E32\u0E01\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## อะไร & ทำไม?

การเขียนไปยัง standard error (stderr) สามารถทำได้โดยการส่งข้อความแสดงข้อผิดพลาดและการวินิจฉัยไปยังกระแส (stream) เฉพาะที่แยกต่างหากจากมาตรฐานการแสดงผล (stdout) ซึ่งช่วยให้การจัดการกับข้อผิดพลาดและการวิเคราะห์บันทึกเป็นไปได้อย่างดียิ่งขึ้น นักพัฒนาทำแบบนี้เพื่อสนับสนุนการดีบักและเพื่อให้แน่ใจว่าข้อความแสดงข้อผิดพลาดสามารถถูกระบุและเปลี่ยนทิศทางได้ง่ายหากจำเป็น เพื่อรักษาบันทึกการแสดงผลหรือข้อความแจ้งผู้ใช้ให้อยู่ในสภาพที่ดี

## วิธีทำ:

ใน Kotlin, การเขียนไปยัง stderr สามารถทำได้โดยใช้ `System.err.println()` วิธีนี้คล้ายกับ `System.out.println()` แต่เป็นการส่งการแสดงผลไปยังกระแสข้อผิดพลาดมาตรฐานแทนที่จะเป็นกระแสการแสดงผลมาตรฐาน

```kotlin
fun main() {
    System.err.println("นี่คือข้อความแสดงข้อผิดพลาด!")
}
```

ตัวอย่างผลลัพธ์:
```
นี่คือข้อความแสดงข้อผิดพลาด!
```

สำหรับแอปพลิเคชันที่มีโครงสร้างมากขึ้นหรือซับซ้อนเป็นพิเศษ โดยเฉพาะอย่างยิ่งที่เกี่ยวข้องกับเฟรมเวิร์กการบันทึกข้อมูลเช่น Logback หรือ SLF4J คุณสามารถกำหนดค่า logger เพื่อเขียนไปยัง stderr สำหรับระดับบันทึกบางประเภท (เช่น ERROR)

การใช้ SLF4J กับ Logback:

1. ขั้นแรก, เพิ่ม SLF4J API และการดำเนินการของ Logback ลงใน `build.gradle` ของคุณ:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. ต่อไป, กำหนดค่า Logback (ใน `src/main/resources/logback.xml`) เพื่อส่งข้อความข้อผิดพลาดไปยัง stderr:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. จากนั้น, ใช้ SLF4J ในโค้ด Kotlin ของคุณเพื่อบันทึกข้อความแสดงข้อผิดพลาด:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("นี่คือข้อความบันทึกข้อผิดพลาด!")
}
```

ตัวอย่างผลลัพธ์ (ไปที่ stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - นี่คือข้อความบันทึกข้อผิดพลาด!
```
