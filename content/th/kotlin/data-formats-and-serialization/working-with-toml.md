---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:06.715246-06:00
description: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious,\
  \ Minimal Language \u0E21\u0E31\u0E19\u0E16\u0E39\u0E01\u0E43\u0E0A\u0E49\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\
  \u0E19\u0E14\u0E04\u0E48\u0E32 \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E21\u0E31\u0E19\u0E07\
  \u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E41\
  \u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E21\
  \u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.205871-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious, Minimal\
  \ Language \u0E21\u0E31\u0E19\u0E16\u0E39\u0E01\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\
  \u0E14\u0E04\u0E48\u0E32 \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E21\u0E31\u0E19\u0E07\u0E48\
  \u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\
  \u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E21\u0E19\
  \u0E38\u0E29\u0E22\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
---

{{< edit_this_page >}}

## อะไรและทำไม?
TOML ย่อมาจาก Tom's Obvious, Minimal Language มันถูกใช้สำหรับไฟล์การกำหนดค่า เพราะมันง่ายต่อการอ่านและเขียนสำหรับมนุษย์ ในขณะที่ยังคงง่ายต่อการแยกวิเคราะห์สำหรับเครื่องจักร นักพัฒนาเลือกใช้ TOML เพื่อหลีกเลี่ยงความยุ่งยากของ XML และความซับซ้อนของ JSON เมื่อต้องจัดการกับการกำหนดค่า

## วิธีการ:
เพื่อจัดการกับ TOML ใน Kotlin คุณอาจใช้ไลบรารีเช่น `ktoml` ขั้นแรก มาเพิ่มการพึ่งพาใน `build.gradle.kts` ของคุณ:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

ตอนนี้ มาประมวลผล TOML กัน:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("Database Host: $host")
    println("Database Port: $port")
}
```

โดยสมมติว่า `config.toml` มีลักษณะดังนี้:

```toml
[database]
host = "localhost"
port = 5432
```

ผลลัพธ์ตัวอย่างจะเป็น:

```
Database Host: localhost
Database Port: 5432
```

## ลงลึก
TOML ถูกคิดค้นขึ้นโดย Tom Preston-Werner ผู้ร่วมก่อตั้ง GitHub ในปี 2013 มีเป้าหมายเพื่อเป็นภาษาที่ง่ายกว่า YAML และไม่ว่างเปล่ากว่า JSON มันเป็นที่นิยม โดยเฉพาะอย่างยิ่งกับระบบ `Cargo` ของ Rust และระบบโมดูลของ Go มีทางเลือกอื่นอย่างไร? YAML มีคุณสมบัติมากกว่า, JSON แปลงโดยตรงเป็นวัตถุในหลายภาษาการเขียนโปรแกรม, และยังมี XML ที่ดีเรื่อยๆ สำหรับการดำเนินการ, ktoml ภายใต้ใบอนุญาต Apache 2.0 เป็นไลบรารี Kotlin แท้ๆ และไม่ลากไลบรารี Java มาด้วย นอกจากนี้ยังนำเสนอ DSL เพื่อเขียน TOML ด้วย เช่นเดียวกับการอ่าน

## ดูเพิ่มเติม
- GitHub ของ TOML: https://github.com/toml-lang/toml
- GitHub ของ ktoml: https://github.com/akuleshov7/ktoml
- เปรียบเทียบ TOML กับ YAML กับ JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
