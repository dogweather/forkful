---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:40.075262-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
YAML, যার অর্থ YAML Ain't Markup Language, একটি খুবই পাঠযোগ্য ডেটা সিরিয়ালাইজেশন ফরম্যাট যা সাধারণত কনফিগারেশন ফাইল, ডেটা স্টোরেজ, এবং ইন্টার-প্রসেস মেসেজিং-এর জন্য ব্যবহৃত হয়। প্রোগ্রামাররা প্রায়শই YAML নিয়ে কাজ করে থাকে যাতে করে তারা কনফিগারেশন এবং সেটিংস সুশৃঙ্খল এবং সরল ভাবে পরিচালনা করতে পারে, যখন পাঠযোগ্যতা গুরুত্বপূর্ণ তখন JSON অথবা XML-এর তুলনায় এর স্পষ্টতা এবং সহজতা থেকে উপকৃত হয়।

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
