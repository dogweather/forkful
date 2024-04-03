---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:22.968473-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin, \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u099F\u09BF\u0995\u09BE\u09B2\u09BF \u099F\
  \u09BE\u0987\u09AA\u09A1 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\
  \u0982 \u09AD\u09BE\u09B7\u09BE \u09AF\u09BE JVM \u098F \u099A\u09B2\u09C7, \u09B8\
  \u09BF\u098F\u09B8\u09AD\u09BF \u09AB\u09BE\u0987\u09B2 \u09B8\u09BE\u09AE\u09B2\
  \u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8 \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\
  \u09B0\u09C7 \u09A8\u09BE\u0964 \u09A4\u09AC\u09C7, \u0986\u09AA\u09A8\u09BF Java\u2026"
lastmod: '2024-03-17T18:47:44.017008-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u099F\
  \u09BF\u0995\u09BE\u09B2\u09BF \u099F\u09BE\u0987\u09AA\u09A1 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AD\u09BE\u09B7\u09BE \u09AF\
  \u09BE JVM \u098F \u099A\u09B2\u09C7, \u09B8\u09BF\u098F\u09B8\u09AD\u09BF \u09AB\
  \u09BE\u0987\u09B2 \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u0995\u09CB\u09A8 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\
  \u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u09A8\u09BE\u0964 \u09A4\u09AC\
  \u09C7, \u0986\u09AA\u09A8\u09BF Java `BufferedReader` \u098F\u09AC\u0982 `FileWriter`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8\u0997\u09C1\u09B2\u09BF \u09AC\u09C7\u09B8\u09BF\
  \u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8, \u0985\u09A5\u09AC\u09BE `kotlinx.serialization` \u098F\
  \u09AC\u0982 `opencsv` \u098F\u09B0 \u09AE\u09A4\u09CB \u099C\u09A8\u09AA\u09CD\u09B0\
  \u09BF\u09AF\u09BC \u09A5\u09BE\u09B0\u09CD\u09A1 \u09AA\u09BE\u09B0\u09CD\u099F\
  \u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\
  \u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 \u0986\u09B0\u09CB \u0989\
  \u09A8\u09CD\u09A8\u09A4 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09CD\u09B7\u09AE\u09A4\
  \u09BE \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09AF\u09BC\u0964\n\n#."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
Kotlin, একটি স্ট্যাটিকালি টাইপড প্রোগ্রামিং ভাষা যা JVM এ চলে, সিএসভি ফাইল সামলানোর জন্য কোন বিল্ট-ইন লাইব্রেরি অন্তর্ভুক্ত করে না। তবে, আপনি Java `BufferedReader` এবং `FileWriter` ক্লাসগুলি বেসিক অপারেশনের জন্য ব্যবহার করতে পারেন, অথবা `kotlinx.serialization` এবং `opencsv` এর মতো জনপ্রিয় থার্ড পার্টি লাইব্রেরিগুলির সাহায্যে আরো উন্নত কার্যক্ষমতা পাওয়া যায়।

### BufferedReader ব্যবহার করে একটি CSV ফাইল পড়া:
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

_নমুনা আউটপুট:_

```
[নাম, বয়স, শহর]
[জন ডো, ৩০, নিউ ইয়র্ক]
[জেন স্মিথ, ২৫, লন্ডন]
```

### FileWriter ব্যবহার করে CSV ফাইলে লেখা:
```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("নাম", "বয়স", "শহর"),
        listOf("জন ডো", "৩০", "নিউ ইয়র্ক"),
        listOf("জেন স্মিথ", "২৫", "লন্ডন")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

এটি `output.csv` নির্দিষ্ট ডেটা দিয়ে তৈরি অথবা উপরে লেখা হবে।

### kotlinx.serialization ব্যবহার করে CSV সিরিয়ালাইজেশন:
প্রথমে, আপনার `build.gradle.kts` এ ডিপেন্ডেন্সি যোগ করুন:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_নোট: সঠিক সংস্করণ এবং রিপোজিটরি কনফিগারেশন নিশ্চিত করুন।_

তারপর, আপনার ডেটা ক্লাস ডিফাইন করুন এবং সিরিয়ালাইজেশনের জন্য `Csv` ফর্ম্যাট ব্যবহার করুন:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class ব্যক্তি(val নাম: String, val বয়স: Int, val শহর: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        ব্যক্তি("জন ডো", ৩০, "নিউ ইয়র্ক"),
        ব্যক্তি("জেন স্মিথ", ২৫, "লন্ডন")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_নমুনা আউটপুট:_

```
জন ডো,৩০,নিউ ইয়র্ক
জেন স্মিথ,২৫,লন্ডন
```

### উন্নত অপারেশনের জন্য OpenCSV ব্যবহার করা:
আপনার প্রজেক্টের ডিপেন্ডেন্সিগুলিতে OpenCSV যোগ করুন:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

OpenCSV সাথে পড়া এবং লেখা:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // CSV পড়া
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // CSV লেখা
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("নাম", "বয়স", "শহর"),
            arrayOf("জন ডো", "৩০", "নিউ ইয়র্ক"),
            arrayOf("জেন স্মিথ", "২৫", "লন্ডন")
        )
        csvWriter.writeAll(entries)
    }
}
```

এই কোড ছোটকাটগুলি দেখায় যে, Kotlin যখন সিএসভি ফাইল নিয়ে কাজ করে তখন কিভাবে আপনার প্রজেক্টের প্রয়োজন অনুসারে সেরা পদ্ধতি চয়ন করার নমনীয়তা দেয়।
