---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:27.077387-06:00
description: "\u098F\u0995\u099F\u09BF \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0\u09C7\
  \ \u09A1\u09C1\u09AC \u09A6\u09C7\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\
  \u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u09AE\u09A7\
  \u09CD\u09AF \u09A6\u09BF\u09DF\u09C7 \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\
  \u09C7 \u098F\u0997\u09BF\u09DF\u09C7 \u09AF\u09BE\u0993\u09DF\u09BE, \u0997\u09BF\
  \u09DF\u09BE\u09B0\u0997\u09C1\u09B2\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u0998\u09C1\u09B0\u099B\u09C7 \u09A4\u09BE \u09A6\u09C7\u0996\u09BE \u098F\u09AC\
  \u0982 \u09B8\u09C7\u0987 \u09AC\u09BF\u09B0\u0995\u09CD\u09A4\u09BF\u0995\u09B0\
  \ \u09AC\u09BE\u0997\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B2\u09BE\u09B2 \u09B9\
  \u09BE\u09A4\u09C7 \u09A7\u09B0\u09BE\u0964\u2026"
lastmod: '2024-03-17T18:47:43.998296-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0\u09C7\
  \ \u09A1\u09C1\u09AC \u09A6\u09C7\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\
  \u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u09AE\u09A7\
  \u09CD\u09AF \u09A6\u09BF\u09DF\u09C7 \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\
  \u09C7 \u098F\u0997\u09BF\u09DF\u09C7 \u09AF\u09BE\u0993\u09DF\u09BE, \u0997\u09BF\
  \u09DF\u09BE\u09B0\u0997\u09C1\u09B2\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u0998\u09C1\u09B0\u099B\u09C7 \u09A4\u09BE \u09A6\u09C7\u0996\u09BE \u098F\u09AC\
  \u0982 \u09B8\u09C7\u0987 \u09AC\u09BF\u09B0\u0995\u09CD\u09A4\u09BF\u0995\u09B0\
  \ \u09AC\u09BE\u0997\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B2\u09BE\u09B2 \u09B9\
  \u09BE\u09A4\u09C7 \u09A7\u09B0\u09BE\u0964\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি ডিবাগারে ডুব দেওয়া মানে হল আপনার কোডের মধ্য দিয়ে ধাপে ধাপে এগিয়ে যাওয়া, গিয়ারগুলি কিভাবে ঘুরছে তা দেখা এবং সেই বিরক্তিকর বাগগুলিকে লাল হাতে ধরা। প্রোগ্রামাররা ডিবাগার ব্যবহার করেন কারণ এগুলো হল সেই গোয়েন্দা টুলগুলি যা আমাদের এই বুঝতে সাহায্য করে যে কোথায় ঝামেলা হচ্ছে আমাদের চুল উপড়ে ফেলা ছাড়া।

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
