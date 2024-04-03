---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:48.682465-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ (JavaScript Object Notation) \u0E43\u0E19 Kotlin \u0E40\u0E01\u0E35\u0E48\u0E22\
  \u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 JSON\u2026"
lastmod: '2024-03-17T21:57:56.203692-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ (JavaScript Object Notation) \u0E43\u0E19 Kotlin \u0E40\u0E01\u0E35\u0E48\u0E22\
  \u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 JSON \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E0A\u0E31\
  \u0E49\u0E19\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E43\u0E19\u0E41\u0E2D\u0E1B\u0E1E\u0E25\
  \u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E37\u0E48\u0E2D\
  \u0E2A\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E40\u0E27\
  \u0E47\u0E1A\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\
  \u0E14\u0E32\u0E22 \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01 JSON \u0E21\
  \u0E35\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E40\u0E1A\u0E32\u0E41\
  \u0E25\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E21\u0E34\u0E15\u0E23\u0E01\u0E31\u0E1A\u0E21\
  \u0E19\u0E38\u0E29\u0E22\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:
Kotlin ไม่มีการสนับสนุนสำหรับ JSON ในตัว แต่ใช้ประโยชน์จากคุณสมบัติที่ทรงพลังของไลบรารีของบุคคลที่สาม เช่น `Gson` โดย Google และ `Kotlinx.serialization` โดย JetBrains นี่คือวิธีที่คุณสามารถใช้ทั้งสองในการทำงานกับ JSON

### การใช้ Gson
เพิ่ม dependency Gson ลงในไฟล์ `build.gradle` ของคุณ:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

วิเคราะห์สตริง JSON เป็นออบเจกต์และในทางกลับกัน:
```kotlin
import com.google.gson.Gson

// กำหนด data class
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serialize
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // ผลลัพธ์: {"name":"John Doe","age":30}

    // Deserialize
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // ผลลัพธ์: User(name=John Doe, age=30)
}
```

### การใช้ Kotlinx.serialization
ก่อนอื่น, รวม dependency ในไฟล์ `build.gradle` ของคุณ:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

หลังจากนั้น, ใช้ plugin `kotlinx-serialization` ที่ด้านบนของสคริปต์การสร้างของคุณ:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

การ serialize และ deserialize ด้วย Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// กำหนด serializable data class
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serialize
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // ผลลัพธ์: {"name":"Jane Doe","age":28}

    // Deserialize
    val user = Json.decodeFromString<User>(json)
    println(user)  // ผลลัพธ์: User(name=Jane Doe, age=28)
}
```

ทั้ง Gson และ Kotlinx.serialization ทำให้การทำงานกับ JSON ในแอปพลิเคชัน Kotlin ง่ายขึ้น การเลือกใช้งานไลบรารีใดไลบรารีหนึ่งขึ้นอยู่กับความต้องการเฉพาะของโปรเจกต์และความชอบส่วนบุคคลของคุณ
