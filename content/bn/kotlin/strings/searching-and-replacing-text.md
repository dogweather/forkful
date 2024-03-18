---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:16:17.932150-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
টেক্সট খোঁজা এবং প্রতিস্থাপন করা যেন স্ট্রিং এর সাথে লুকোচুরি খেলা, এরপর লুকিয়ে থাকা ব্যক্তিকে অন্য কারো সাথে পরিবর্তন করা। এটি একটি সাধারণ প্রোগ্রামিং কাজ, বাল্ক এডিটিং, ডেটা স্যানিটাইজেশান এবং বিরক্তিকর কাজগুলি অটোমেট করার মতো কাজের জন্য অপরিহার্য।

## কিভাবে:
কোটলিন এর স্ট্যান্ডার্ড লাইব্রেরির মাধ্যমে টেক্সট ম্যানিপুলেশন সহজ হয়। নিচে দেখুন, কিভাবে `replace` ব্যবহার করে শব্দ পরিবর্তন করা যায়।

```kotlin
fun main() {
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = originalText.replace("pragmatic", "cool")

    println(newText) // আউটপুট: Kotlin is fun, Kotlin is cool!
}
```

রেগেক্স প্যাটার্নের জন্য:

```kotlin
fun main() {
    val regex = "Kotlin".toRegex()
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = regex.replace(originalText, "Java")

    println(newText) // আউটপুট: Java is fun, Java is pragmatic!
}
```

## গভীর ডাইভ
প্রিন্টের সময় থেকেই টেক্সট পুনর্লিখন চলে আসছে, কিন্তু প্রোগ্রামিং-এ, এর ব্যবহার শুরু হয় প্রাথমিক টেক্সট প্রসেসরের সাথে। বিকল্প? অবশ্যই – এডিটরের মধ্যে খুঁজে পাওয়া এবং প্রতিস্থাপন করার ফাংশন, `sed` এর মতো কমান্ড-লাইন টুলস। বিশেষ করে কোটলিনে, আপনার কাছে রেগেক্স এবং প্লেইন স্ট্রিং মেথড উপলব্ধ।

`replace` সহজ টেক্সটের জন্য সোজা; `Regex` আপনাকে প্যাটার্নের জন্য একটি সুইস আর্মি ছুরি প্রদান করে। রেগেক্স শক্তিশালী কিন্তু বিচক্ষণ – তারা প্যাটার্ন ম্যাচ করার জন্য বিশেষ সিনট্যাক্স ব্যবহার করে। রেগেক্স সম্পর্কে ভাবুন যেন আপনি ওয়াল্ডো খুঁজছেন, কিন্তু আপনি ওয়াল্ডো কি পরিধান করে তার নিয়ম তৈরি করছেন।

বাস্তবায়নের সতর্কতা? মনে রাখবেন, কোটলিনের `String` অপরিবর্তনীয়। টেক্সট পরিবর্তনের মেথড নতুন স্ট্রিং রিটার্ন করে; তারা মূল স্ট্রিং পরিবর্তন করে না।

## আরও দেখুন
- কোটলিন ডকুমেন্টেশন `replace` প্রসঙ্গে: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- কোটলিনে রেগেক্স: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- পুরানো ভালো `sed`: https://www.gnu.org/software/sed/manual/sed.html
