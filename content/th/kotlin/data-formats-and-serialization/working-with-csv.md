---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:44.356979-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV\
  \ (Comma-Separated Values) \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\
  \u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E1B\u0E22\
  \u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C CSV,\u2026"
lastmod: '2024-03-17T21:57:56.204942-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV (Comma-Separated\
  \ Values) \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E44\
  \u0E1F\u0E25\u0E4C CSV, \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\u0E1A\u0E15\u0E32\u0E23\u0E32\u0E07\u0E43\u0E19\
  \u0E23\u0E39\u0E1B\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E18\u0E23\u0E23\u0E21\
  \u0E14\u0E32 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\u0E1F\u0E25\u0E4C CSV \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E41\u0E2D\u0E1B\
  \u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E15\u0E48\u0E32\u0E07 \u0E46\
  , \u0E10\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E2B\u0E23\u0E37\u0E2D\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E07\
  \u0E32\u0E19\u0E14\u0E49\u0E32\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\
  \u0E25\u0E1C\u0E25\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E14\u0E49\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:
Kotlin เป็นภาษาโปรแกรมที่เป็น static type ที่ทำงานบน JVM ไม่มีไลบรารีในตัวสำหรับการจัดการไฟล์ CSV อย่างไรก็ตาม คุณสามารถใช้คลาส `BufferedReader` และ `FileWriter` ของ Java สำหรับการดำเนินการพื้นฐาน หรือใช้ไลบรารีของบุคคลที่สามที่ได้รับความนิยม เช่น `kotlinx.serialization` และ `opencsv` เพื่อฟังก์ชั่นการทำงานขั้นสูง

### การอ่านไฟล์ CSV โดยใช้ BufferedReader:
```kotlin
import java.io.BufferedReader
import java.io.FileReader

fun main() {
    val path = "data.csv"
    val br = BufferedReader(FileReader(path))
    br.useLines { lines ->
        lines.forEach { line ->
            val cols = line.split(',')
            println(cols)
        }
    }
}
```

_ตัวอย่างผลลัพธ์:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### การเขียนไปยังไฟล์ CSV โดยใช้ FileWriter:
```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

นี่จะสร้างหรือเขียนทับ `output.csv` ด้วยข้อมูลที่ให้มา

### การใช้ kotlinx.serialization สำหรับการ serialize CSV:
เริ่มต้นด้วยการเพิ่ม dependency ลงใน `build.gradle.kts` ของคุณ:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_หมายเหตุ: ตรวจสอบให้แน่ใจว่าคุณมีเวอร์ชันที่ถูกต้องและการตั้งค่า repository_

จากนั้น กำหนด data class ของคุณและใช้ฟอร์แมต `Csv` สำหรับการ serialize:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Person(val name: String, val age: Int, val city: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Person("John Doe", 30, "New York"),
        Person("Jane Smith", 25, "London")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_ตัวอย่างผลลัพธ์:_

```
John Doe,30,New York
Jane Smith,25,London
```

### การใช้ OpenCSV สำหรับการดำเนินการขั้นสูง:
เพิ่ม OpenCSV เข้าไปใน dependencies ของโปรเจคของคุณ:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

การอ่านและเขียนด้วย OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // การอ่าน CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // การเขียน CSV
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        csvWriter.writeAll(entries)
    }
}
```

ตัวอย่างเหล่านี้แสดงให้เห็นถึงความยืดหยุ่นที่ Kotlin มอบให้เมื่อทำงานกับไฟล์ CSV ช่วยให้คุณสามารถเลือกวิธีการที่เหมาะสมที่สุดกับความต้องการของโปรเจคของคุณ
