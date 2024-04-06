---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:18.326654-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Kotlin \u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23 HTTP requests \u0E14\u0E49\u0E27\u0E22\u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E40\u0E0A\u0E48\u0E19 `ktor` \u0E2B\u0E23\u0E37\u0E2D `okhttp`\
  \ \u0E21\u0E32\u0E25\u0E2D\u0E07\u0E43\u0E0A\u0E49 `okhttp` \u0E01\u0E31\u0E19\u0E43\
  \u0E19\u0E15\u0E2D\u0E19\u0E19\u0E35\u0E49 \u0E43\u0E2B\u0E49\u0E17\u0E33\u0E01\u0E32\
  \u0E23\u0E14\u0E36\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E43\u0E19\u0E44\
  \u0E1F\u0E25\u0E4C build.gradle \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
lastmod: '2024-04-05T21:54:01.828121-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23 HTTP requests \u0E14\u0E49\u0E27\
  \u0E22\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E40\u0E0A\u0E48\u0E19 `ktor`\
  \ \u0E2B\u0E23\u0E37\u0E2D `okhttp` \u0E21\u0E32\u0E25\u0E2D\u0E07\u0E43\u0E0A\u0E49\
  \ `okhttp` \u0E01\u0E31\u0E19\u0E43\u0E19\u0E15\u0E2D\u0E19\u0E19\u0E35\u0E49 \u0E43\
  \u0E2B\u0E49\u0E17\u0E33\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C build.gradle \u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
Kotlin จัดการ HTTP requests ด้วยไลบรารีเช่น `ktor` หรือ `okhttp` มาลองใช้ `okhttp` กันในตอนนี้

ให้ทำการดึงไลบรารีในไฟล์ build.gradle ของคุณ:

```groovy
dependencies {
    implementation("com.squareup.okhttp3:okhttp:4.9.0")
}
```

ถึงเวลาเขียนโค้ด:

```kotlin
import okhttp3.Credentials
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException

fun main() {
    val client = OkHttpClient()

    val username = "admin"
    val password = "password123"
    val credentials = Credentials.basic(username, password)

    val request = Request.Builder()
        .url("http://example.com/resource")
        .header("Authorization", credentials)
        .build()

    client.newCall(request).execute().use { response ->
        if (!response.isSuccessful) throw IOException("Unexpected code $response")

        println(response.body!!.string())
    }
}
```

กดรันและมองหาข้อมูลที่ปลอดภัยที่เราต้องการจากคอนโซลของคุณ เราควรจะเห็นข้อมูลนั้นถูกแสดงออกมา

## ลงลึก
ในอดีต HTTP Basic Auth เป็นที่นิยม ง่ายๆ เพียงแค่ base64 `username:password` และใส่ลงในส่วนหัว ไม่ปลอดภัยเมื่อใช้คนเดียว ดังนั้น HTTPS จึงได้เข้ามามีบทบาท

มีทางเลือกอื่นๆ มากมาย เช่น OAuth สำหรับโทเค็น, API keys ที่ความเรียบง่าย, หรือการยืนยันตัวตนแบบ digest สำหรับการอัพเกรด การยืนยันตัวตนพื้นฐานดีสำหรับการเริ่มต้นหรือสำหรับเครื่องมือภายใน แต่ไม่เหมาะสำหรับเว็บที่ตระหนักถึงความปลอดภัยในยุคสมัยใหม่

รายละเอียดการประยุกต์ใช้: อย่าไปออกแบบใหม่ ให้ไลบรารีจัดการกับการเข้ารหัสและซับซ้อนของโปรโตคอล OkHttp ยังช่วยจัดการกับการลองใหม่และการเชื่อมต่อสำหรับคุณ จำไว้ว่า การยืนยันตัวตนพื้นฐานผ่าน HTTP เป็นสิ่งที่ไม่ควรทำ—เสมอใช้ HTTPS เพื่อรักษาข้อมูลปลอดภัยขณะส่งผ่าน

## ดูเพิ่มเติม
- เอกสารการใช้งานอย่างเป็นทางการของ OkHttp: [https://square.github.io/okhttp/](https://square.github.io/okhttp/)
- หน้า Kotlin language (สำหรับทุกอย่างเกี่ยวกับ Kotlin): [https://kotlinlang.org/](https://kotlinlang.org/)
- ศึกษาเพิ่มเติมเกี่ยวกับ Basic Auth: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- ทางเลือกต่างๆ ของ Basic Auth เช่น OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)
