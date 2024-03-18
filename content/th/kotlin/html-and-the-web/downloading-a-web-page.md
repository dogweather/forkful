---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:34.494168-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E2D\u0E32 HTML \u0E08\u0E32\u0E01 URL \u0E17\u0E35\u0E48\
  \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E21\u0E32\u0E14\u0E39\u0E2B\u0E23\u0E37\u0E2D\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19\u0E43\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32\u0E40\u0E2D\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E0A\
  \u0E48\u0E19\u0E01\u0E32\u0E23\u0E40\u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E35\u0E1B\
  \u0E15\u0E34\u0E49\u0E07,\u2026"
lastmod: '2024-03-17T21:57:56.173971-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E2D\u0E32 HTML \u0E08\u0E32\u0E01 URL \u0E17\u0E35\u0E48\
  \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E21\u0E32\u0E14\u0E39\u0E2B\u0E23\u0E37\u0E2D\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19\u0E43\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32\u0E40\u0E2D\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E0A\
  \u0E48\u0E19\u0E01\u0E32\u0E23\u0E40\u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E35\u0E1B\
  \u0E15\u0E34\u0E49\u0E07,\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การดาวน์โหลดเว็บเพจหมายถึงการเอา HTML จาก URL ที่กำหนดมาดูหรือใช้งานในเครื่องของเราเอง โปรแกรมเมอร์ทำสิ่งนี้เพื่ออย่างเช่นการเว็บสกรีปติ้ง, การอ่านแบบออฟไลน์, หรือการทดสอบอัตโนมัติ

## วิธีการ:
มาเริ่มใช้ `HttpURLConnection` ของ Kotlin ในการดึงเว็บเพจอย่างรวดเร็วกันเถอะ เราจะใช้ coroutines ในการทำงานเบื้องหลังอย่างราบรื่น นี่คือคู่มือ:

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import kotlinx.coroutines.*

fun main() = runBlocking {
    val url = "http://example.com"
    val result = withContext(Dispatchers.IO) {
        downloadWebPage(url)
    }
    println(result)
}

fun downloadWebPage(urlAddress: String): String {
    val url = URL(urlAddress)
    val connection = url.openConnection() as HttpURLConnection
    try {
        connection.connect()
        return connection.inputStream.bufferedReader().use { it.readText() }
    } finally {
        connection.disconnect()
    }
}
```

ตัวอย่างผลลัพธ์:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```
ดีไหม? คุณได้ HTML ของเว็บเพจแล้วนะ!

## การทำความเข้าใจลึกซึ้ง
การดาวน์โหลดเว็บเพจเป็นเรื่องที่เกิดขึ้นมานานแล้วตั้งแต่ยุคของเว็บเอง ในยุค 90, คนๆ ใช้เครื่องมือบรรทัดคำสั่งอย่าง `wget` และ `curl` พวกมันยังคงมีอยู่แต่เมื่อคุณต้องการควบคุมมากขึ้นหรือต้องการผสมผสานการดึงเนื้อหาเว็บเข้ากับแอพ, คุณจะต้องเขียนโค้ดมัน

ใน Kotlin, คุณสามารถใช้ `HttpURLConnection` ของ Java หรือไลบรารีอย่าง OkHttp หรือ Ktor สำหรับวิธีที่มีประสิทธิภาพมากขึ้นด้วยคุณสมบัติเพิ่มเติม ตัวอย่างข้างต้นเป็นเพียงพื้นฐาน; ในชีวิตจริง, คุณต้องพิจารณาเรื่องการจัดการข้อผิดพลาด, เรื่องการเปลี่ยนทิศทาง, และประสิทธิภาพ อาจจะเพิ่มการลองใหม่หรือเวลาหมดอายุ? และคุณไม่สามารถลืมเกี่ยวกับการจัดการรหัสตัวละครและประเภทเนื้อหาที่แตกต่างกันได้

คุณก็ต้องคิดถึงการทำงานของเธรด ไม่ต้องการให้เธรดหลักหยุดทำงานในขณะที่กำลังดึงหน้าเว็บขนาดใหญ่ใช่ไหม? นั่นคือเหตุผลของ coroutines - ช่วยให้แอพของคุณยังคงตอบสนองได้, ดึงข้อมูลในเบื้องหลังโดยไม่ต้องทำงานหนัก

## ดูอีกด้วย
- **OkHttp**: https://square.github.io/okhttp/
- **Ktor Client**: https://ktor.io/docs/client.html
- **Kotlin Coroutines**: https://kotlinlang.org/docs/coroutines-overview.html
- **Java HttpURLConnection**: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html

นั่นคือความสำคัญ—ได้หน้าเว็บมา, ปฏิบัติอย่างฉลาดเกี่ยวกับมัน, และเคารพต่อข้อมูลและแหล่งที่มาเสมอ สนุกกับการเขียนโค้ดนะ!
