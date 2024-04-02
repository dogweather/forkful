---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:27.077387-06:00
description: "\u098F\u0996\u09BE\u09A8\u09C7 IntelliJ IDEA - IDE \u098F\u09B0 \u09B6\
  \u09BE\u09B0\u09CD\u09B2\u0995 \u09B9\u09CB\u09AE\u09B8\u09C7\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 Kotlin-\u098F \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u098F\u09B0\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09CD\u09AC\u09BE\u09A6 \u09A6\u09C7\u0993\u09DF\
  \u09BE \u09B9\u09B2\u0983 ```kotlin fun main() { val mysteryNumber = 42 var guess\
  \ = 0 while\u2026"
lastmod: '2024-03-17T18:47:43.998296-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 IntelliJ IDEA - IDE \u098F\u09B0 \u09B6\u09BE\
  \u09B0\u09CD\u09B2\u0995 \u09B9\u09CB\u09AE\u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 Kotlin-\u098F \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u098F\u09B0 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u09AC\u09BE\u09A6 \u09A6\u09C7\u0993\u09DF\u09BE\
  \ \u09B9\u09B2\u0983 ```kotlin fun main() { val mysteryNumber = 42 var guess = 0\
  \ while\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
এখানে IntelliJ IDEA - IDE এর শার্লক হোমসের সাথে Kotlin-এ ডিবাগিং এর একটি স্বাদ দেওয়া হলঃ

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("সংখ্যা অনুমান করুন: ")
        guess = readLine()?.toIntOrNull() ?: continue // খারাপ ইনপুট উপেক্ষা করুন

        // এখানে একটি ব্রেকপয়েন্ট সেট করুন যাতে 'guess' কর্মে দেখতে পান
        if (guess < mysteryNumber) {
            println("খুব কম!")
        } else if (guess > mysteryNumber) {
            println("খুব বেশি!")
        }
    }

    println("আপনি পেয়ে গেছেন! রহস্যময় সংখ্যাটি ছিল $mysteryNumber")
}
```

ডিবাগার আউটপুট:
```
সংখ্যা অনুমান করুন: 
10
খুব কম!
সংখ্যা অনুমান করুন: 
50
খুব বেশি!
সংখ্যা অনুমান করুন: 
42
আপনি পেয়ে গেছেন! রহস্যময় সংখ্যাটি ছিল 42
```

## গভীর ডুব
ডিবাগার সাথে খেলা শুরু হয়েছিল '৫০ এর দশকে। সে সময়ে, তারা বেশ প্রাথমিক ছিল, এবং ডিবাগিং হতে পারে হার্ডওয়্যারের বিষয়ে বেশি থাকতে। বর্তমানে, IntelliJ IDEA এর মত একটি ডিবাগার আমাদের ব্রেকপয়েন্ট সেট করতে, কোড লাইন ধরে ধরে এগিয়ে যেতে এবং আমাদের সুবিধামত ভেরিয়েবলের অবস্থা পরীক্ষা করতে দেয়।

যদিও IntelliJ-এর ডিবাগার Kotlin-এর জন্য অত্যন্ত উপযোগী, তবে এটি সমুদ্রের একমাত্র মাছ নয়। Android ডেভেলপমেন্টের জন্য Logcat এর মত বিকল্পগুলো রয়েছে, অথবা সরলতার জন্য কমান্ড-লাইন টুলস যেমন jdb রয়েছে। এখানে অধীনস্ত জাদু বেশিরভাগই JVM Tool Interface (JVMTI) সম্পর্কে, যা ডিবাগারদের জাভা ভার্চুয়াল মেশিনের সাথে মিথস্ক্রিয়া করতে দেয়, কোটলিন ডেভেলপারদের লুপে রাখে।

## দেখুন আরও
- IntelliJ IDEA Debugger ডকুমেন্টেশন: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
