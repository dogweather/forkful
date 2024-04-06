---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:58.098384-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u09CB\u099F\u09B2\u09BF\
  \u09A8\u09C7\u09B0 \u09A8\u09BF\u099C\u09C7\u09B0 \u0995\u09CB\u09A8 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09DF API \u09A8\u09C7\u0987\
  , \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u098F\u099F\u09BF \u098F\u0987 \u09AB\u09BE\
  \u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u099C\u09BE\u09AD\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0995\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u0995\u09C0\u09AD\u09BE\u09AC\u09C7\
  \ \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.004932-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u099F\u09B2\u09BF\u09A8\u09C7\u09B0 \u09A8\u09BF\u099C\u09C7\
  \u09B0 \u0995\u09CB\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\
  \u09AE\u09DF API \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u098F\
  \u099F\u09BF \u098F\u0987 \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\
  \u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u099C\u09BE\u09AD\u09BE \u09B8\u09CD\u099F\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\
  \ \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF\
  \ \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u09A8."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:


### স্ট্যান্ডার্ড কোটলিন ব্যবহার করে
কোটলিনের নিজের কোন তারিখ এবং সময় API নেই, কিন্তু এটি এই ফাংশনালিটির জন্য জাভা স্ট্যান্ডার্ড লাইব্রেরিকে নির্ভর করে। এখানে আপনি কীভাবে বর্তমান তারিখ পেতে পারেন:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("আজকের তারিখ: $today")
}
```

**নমুনা আউটপুট:**
```
আজকের তারিখ: 2023-04-05
```

### java.util.Date ব্যবহার করে
তারিখ এবং সময় উভয়ই প্রয়োজন হলে, আপনি হয়ত `java.util.Date` পছন্দ করতে পারেন।

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("বর্তমান তারিখ ও সময়: $currentDate")
}
```

**নমুনা আউটপুট:**
```
বর্তমান তারিখ ও সময়: বুধ এপ্রি 05 15:20:45 GMT 2023
```

### Joda-Time লাইব্রেরি ব্যবহার করে
জাভা 8 নতুন তারিখ এবং সময় API চালু করার আগে, Joda-Time জাভা এবং কোটলিনে তারিখ-সময় অপারেশনের জন্য ডি-ফ্যাক্টো স্ট্যান্ডার্ড ছিল। যদিও এটি অনেক প্রজেক্টের জন্য আর প্রয়োজনীয় নয়, কিছু প্রজেক্ট এটি লেগ্যাসি কারণ বা ব্যক্তিগত পছন্দের জন্য এখনও ব্যবহার করতে পারে।

আপনার প্রকল্পের build.gradle ফাইলে Joda-Time লাইব্রেরি যোগ করুন:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("আজকের তারিখ: $today")
}
```

**নমুনা আউটপুট:**
```
আজকের তারিখ: 2023-04-05
```

### অ্যান্ড্রয়েডের জন্য ThreeTenABP ব্যবহার করা
অ্যান্ড্রয়েড ডেভেলপমেন্টের জন্য, অ্যান্ড্রয়েড API লেভেল 26 এর আগের সংস্করণের জন্য Java Time API এর ব্যাকপোর্ট ব্যবহার করা হিসেবে ThreeTen Android Backport Project ব্যবহার করা প্রস্তাবিত।

আপনার অ্যাপের build.gradle ফাইলে ডিপেন্ডেন্সি যোগ করুন:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

এটি আপনার Application ক্লাসে ইনিশিয়ালাইজ করুন:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

তারপর, আপনি এটি এরকম ব্যবহার করতে পারেন:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("আজকের তারিখ: $today")
}
```

**নমুনা আউটপুট:**
```
আজকের তারিখ: 2023-04-05
```
