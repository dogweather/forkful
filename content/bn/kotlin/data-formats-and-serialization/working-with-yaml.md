---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:40.075262-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u098F YAML \u09AA\u09BE\
  \u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u09B8\u09BF\u09B0\u09BF\u09AF\
  \u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4\
  \ \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\
  \u09A4\u09C1 \u0986\u09AA\u09A8\u09BF `snakeyaml` (\u09B8\u09BE\u09A7\u09BE\u09B0\
  \u09A3 YAML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982-\u098F\u09B0 \u099C\u09A8\
  \u09CD\u09AF) \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.015050-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u098F YAML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\
  \u0982 \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\
  \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\
  \ \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u09AA\u09A8\u09BF\
  \ `snakeyaml` (\u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 YAML \u09AA\u09BE\u09B0\u09CD\
  \u09B8\u09BF\u0982-\u098F\u09B0 \u099C\u09A8\u09CD\u09AF) \u098F\u09AC\u0982 `kotlinx.serialization`\
  \ (\u098F\u0995\u099F\u09BF YAML \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u098F\
  \u0995\u09CD\u09B8\u099F\u09C7\u09A8\u09B6\u09A8\u09B8\u09B9) \u09AE\u09A4\u09CB\
  \ \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09A5\u09BE\u09B0\u09CD\u09A1\
  -\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 YAML \u09AB\u09BE\u0987\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\
  \n\n**\u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\u09B6\u09C0\u09B2\u09A4\u09BE:**."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
Kotlin এ YAML পার্সিং এবং সিরিয়ালাইজেশনের জন্য অন্তর্নির্মিত সমর্থন নেই, কিন্তু আপনি `snakeyaml` (সাধারণ YAML পার্সিং-এর জন্য) এবং `kotlinx.serialization` (একটি YAML ফরম্যাট এক্সটেনশনসহ) মতো জনপ্রিয় থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করে YAML ফাইল নিয়ে কাজ করতে পারেন।

### `snakeyaml` ব্যবহার করে
**নির্ভরশীলতা:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**YAML পড়ুন:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// নমুনা ব্যবহার
fun main() {
    readYaml("config.yaml")
}
```
**নমুনা `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**নমুনা আউটপুট:**
```
{database={host=localhost, port=5432}}
```

### `kotlinx.serialization` ব্যবহার করে YAML
প্রথমে, আপনি যদি একটি উপযুক্ত YAML সমর্থন লাইব্রেরির সাথে `kotlinx-serialization` লাইব্রেরি আছে তা নিশ্চিত করুন (যেহেতু `kotlinx.serialization` মূলত JSON এবং অন্যান্য ফরম্যাটের দিকে লক্ষ্য রাখে)।

**নির্ভরশীলতা:**
```kotlin
// JSON-এর জন্য (ব্যাখ্যামূলক, YAML সমর্থনের জন্য অথবা বিকল্প লাইব্রেরিগুলি পরীক্ষা করুন)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**একটি সিরিয়ালাইজেবল ডেটা ক্লাস নির্ধারণ করুন:**
```kotlin
import kotlinx.serialization.Serializable

@Serializable
data class Config(
    val database: Database
)

@Serializable
data class Database(
    val host: String,
    val port: Int
)
```

লেখার সময়ে, সরাসরি YAML সমর্থন `kotlinx.serialization`-এ সীমিত অথবা বিবর্তনশীল হতে পারে। আপনাকে `snakeyaml` দিয়ে YAML-কে JSON-এ পরিবর্তন করে তারপর `kotlinx.serialization` দিয়ে JSON পার্স করার মতো মধ্যবর্তী প্রকাশ ব্যবহার করতে হতে পারে অথবা `kotlinx.serialization`-এর সাথে সামঞ্জস্যপূর্ণ কমিউনিটি-চালিত YAML সিরিয়ালাইজেশন প্রোজেক্ট খুঁজতে হতে পারে।

JSON-এর জন্য, কোডটি এরকম দেখাবে:
```kotlin
import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString

fun main() {
    val jsonText = """
    {
        "database": {
            "host": "localhost",
            "port": 5432
        }
    }
    """.trimIndent()
    
    val config = Json.decodeFromString<Config>(jsonText)
    println(config)
}
```

Kotlin এবং এর ইকোসিস্টেম যেমন বিবর্তনমান রয়েছে, YAML সমর্থন এবং লাইব্রেরিগুলির সর্বশেষের জন্য অফিসিয়াল ডকুমেন্টেশন এবং কমিউনিটি রিসোর্স এর সাথে সংযুক্ত থাকুন।
