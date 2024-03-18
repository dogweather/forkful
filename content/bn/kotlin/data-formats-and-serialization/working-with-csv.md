---
title:                "CSV এর সাথে কাজ করা"
date:                  2024-03-17T18:28:22.968473-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

CSV (Comma-Separated Values) নিয়ে কাজ করা মানে সিএসভি ফাইল থেকে ডেটা পড়া এবং সেখানে ডেটা লেখার কাজ করা, যা সাধারণত সারণিবদ্ধ ডেটা সাধারণ পাঠ্যে সঞ্চয় করার জন্য একটি প্রচলিত ফরম্যাট। প্রোগ্রামাররা ভিন্ন অ্যাপ্লিকেশন, ডাটাবেসের মধ্যে ডেটা আদান-প্রদান অথবা ডেটা প্রক্রিয়াকরণ এবং বিশ্লেষণ কাজ সহজ করার জন্য সিএসভি ফাইল নিয়ে কাজ করে থাকেন।

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
