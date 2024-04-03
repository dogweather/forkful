---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:34.491493-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u098F TOML \u09A8\u09BF\
  \u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF, \u0986\u09AA\u09A8\u09BF `ktoml` \u098F\u09B0 \u09AE\u09A4\u09CB \u098F\u0995\
  \u099F\u09BE \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u09A8\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0\
  \ `build.gradle.kts` \u09AB\u09BE\u0987\u09B2\u09C7 \u09A8\u09BF\u09B0\u09CD\u09AD\
  \u09B0\u09A4\u09BE \u09AF\u09CB\u0997 \u0995\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.018001-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u098F TOML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BF `ktoml` \u098F\
  \u09B0 \u09AE\u09A4\u09CB \u098F\u0995\u099F\u09BE \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 `build.gradle.kts` \u09AB\u09BE\u0987\u09B2\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\u09A4\u09BE \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09BE \u09AF\u09BE\u0995."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
Kotlin এ TOML নিয়ে কাজ করার জন্য, আপনি `ktoml` এর মতো একটা লাইব্রেরি ব্যবহার করতে পারেন। প্রথমে, আপনার `build.gradle.kts` ফাইলে নির্ভরতা যোগ করা যাক:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

এখন, আসুন কিছু TOML পার্স করি:

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

ধরা যাক, `config.toml` এইরকম দেখাচ্ছে:

```toml
[database]
host = "localhost"
port = 5432
```

নমুনা আউটপুট হবে:

```
Database Host: localhost
Database Port: 5432
```

## গভীর ডুব
TOML, ২০১৩ সালে GitHub এর সহ-প্রতিষ্ঠাতা Tom Preston-Werner দ্বারা তৈরি, YAML এর চেয়ে সরল এবং JSON এর চেয়ে টাইপ-নিরাপদ হতে লক্ষ্য করেছিল। Rust এর `Cargo` এবং Go এর মডিউল সিস্টেমের সাথে এটি বিশেষভাবে জনপ্রিয় হয়ে উঠেছে। বিকল্প? YAML এর আরও বেশি বৈশিষ্ট্য আছে, JSON অনেক কোডিং ভাষায় সরাসরি অবজেক্টে পরিণত হয়, এবং XML সবসময় একটি ভালো বিকল্প। বাস্তবায়নে, ktoml, Apache 2.0 লাইসেন্সের অধীনে, একটি শুদ্ধ Kotlin লাইব্রেরি এবং Java লাইব্রেরীগুলি নিয়ে আসে না, পাঠন মাত্র নয়, লেখার জন্যও DSL সরবরাহ করে।

## আরও দেখুন
- TOML GitHub: https://github.com/toml-lang/toml
- ktoml GitHub: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
