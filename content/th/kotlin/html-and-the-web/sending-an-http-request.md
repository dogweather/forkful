---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:00.944211-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E43\
  \u0E2B\u0E49\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2B\
  \u0E23\u0E37\u0E2D\u0E43\u0E2B\u0E49\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E01\u0E31\u0E1A\u0E04\u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E2D\u0E23\u0E4C\u0E27\u0E34\u0E2A, \u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E2A\u0E48\u0E07\u0E41\u0E1A\u0E1A\
  \u0E1F\u0E2D\u0E23\u0E4C\u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.171711-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E43\
  \u0E2B\u0E49\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2B\
  \u0E23\u0E37\u0E2D\u0E43\u0E2B\u0E49\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E01\u0E31\u0E1A\u0E04\u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E2D\u0E23\u0E4C\u0E27\u0E34\u0E2A, \u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E2A\u0E48\u0E07\u0E41\u0E1A\u0E1A\
  \u0E1F\u0E2D\u0E23\u0E4C\u0E21,\u2026"
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การส่งคำขอ HTTP เหมือนกับการขอให้เว็บเซิร์ฟเวอร์ทำบางอย่างหรือให้บางอย่างกับคุณ โปรแกรมเมอร์ทำเช่นนี้เพื่อโต้ตอบกับเว็บเซอร์วิส, ดึงข้อมูล, ส่งแบบฟอร์ม, หรือสื่อสารกับ API ต่าง ๆ

## วิธีทำ:

Kotlin ทำให้การร้องขอ HTTP เป็นเรื่องง่าย นี่คือตัวอย่างพื้นฐานโดยใช้ `khttp`, ไลบรารีที่ใช้งานง่าย:

```Kotlin
import khttp.get

fun main() {
    val response = get("https://api.github.com/users/octocat/orgs")
    println(response.text)
}
```

ผลลัพธ์:

```Kotlin
[{"login":"octo-org","id":583231,"url":"https://api.github.com/orgs/octo-org", ...}]
```

สำหรับความต้องการที่มากขึ้น, นี่คือตัวอย่างโค้ดโดยใช้ `ktor`, เฟรมเวิร์กของ Kotlin, เพื่อดึงข้อมูลอย่างไม่ต้องรอตอบสนอง:

```Kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient(CIO)
    val response: String = client.get("https://api.github.com/users/octocat/orgs")
    println(response)
    client.close()
}
```

ผลลัพธ์คล้ายกับตัวอย่างแรก

## ลงลึก

ไลบรารี `khttp` เป็นเครื่องมือที่สะดวก, ได้รับการออกแบบตาม `requests` ใน Python ทำให้เป็นเหมาะสำหรับสคริปต์ขนาดเล็กแต่ไม่ได้รับการดูแลอย่างต่อเนื่อง `ktor` เป็นโปรเจกต์ใหม่ที่ยังมีการพัฒนาอย่างต่อเนื่องโดย JetBrains, ถูกออกแบบมาเพื่อการใช้งานอะซิงโครนัสด้วยโครูตีน มีไว้สำหรับแอปที่ขยายได้ ทั้งสองจัดการกับคำขอ HTTP แต่รองรับกรณีการใช้งานที่แตกต่างกัน

ในอดีต, การร้องขอ HTTP ใน Kotlin ทำเลียบเคียงกับไลบรารีของ Java เช่น `HttpURLConnection` หรือ `HttpClient` ของ Apache ซึ่งยังถือว่าใช้ได้แต่มีความละเอียดและไม่มีคุณสมบัติของภาษา Kotlin

สำหรับการนำไปใช้งาน, จำไว้ว่าต้องจัดการกับข้อผิดพลาด HTTP ทั่วไปและอ่านรหัสตอบกลับ คุณจะต้องการใช้ `try-catch` สำหรับข้อยกเว้นทางเครือข่าย และอาจต้องการทำงานกับหัวข้อและพารามิเตอร์การค้นหา

## ดูเพิ่มเติม

- เอกสารการใช้งาน Ktor: https://ktor.io/
- ที่เก็บข้อมูล GitHub ของ khttp: https://github.com/jkcclemens/khttp (หมายเหตุสถานะการดูแลรักษา)
- การเรียก HTTP ด้วย HttpURLConnection ใน Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/java.net/-http-u-r-l-connection/
