---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:55.431915-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0986\
  \u09AE\u09BE\u09A6\u09C7\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AC\u09CD\
  \u09AF\u09AC\u09B8\u09CD\u09A5\u09BE\u0995\u09C7 \u09A8\u09C7\u0997\u09C7\u099F\u09BF\
  \u09AD \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09AC\u09B0\u09CD\u0997\u09AE\
  \u09C2\u09B2\u09B8\u09AE\u09C2\u09B9 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\
  \u0995\u09CD\u09A4 \u0995\u09B0\u09A4\u09C7 \u09B8\u09AE\u09CD\u09AA\u09CD\u09B0\
  \u09B8\u09BE\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09C7\u0996\u09BE\u09A8\
  \u09C7 '\u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995' \u0987\u0989\u09A8\u09BF\
  \u099F i \u09B9\u09B2\u09CB -1 \u098F\u09B0 \u09AC\u09B0\u09CD\u0997\u09AE\u09C2\
  \u09B2\u0964\u2026"
lastmod: '2024-03-17T18:47:43.987232-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0986\u09AE\
  \u09BE\u09A6\u09C7\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AC\u09CD\u09AF\
  \u09AC\u09B8\u09CD\u09A5\u09BE\u0995\u09C7 \u09A8\u09C7\u0997\u09C7\u099F\u09BF\u09AD\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09AC\u09B0\u09CD\u0997\u09AE\u09C2\
  \u09B2\u09B8\u09AE\u09C2\u09B9 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\
  \u09CD\u09A4 \u0995\u09B0\u09A4\u09C7 \u09B8\u09AE\u09CD\u09AA\u09CD\u09B0\u09B8\
  \u09BE\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09C7\u0996\u09BE\u09A8\u09C7\
  \ '\u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995' \u0987\u0989\u09A8\u09BF\u099F\
  \ i \u09B9\u09B2\u09CB -1 \u098F\u09B0 \u09AC\u09B0\u09CD\u0997\u09AE\u09C2\u09B2\
  \u0964\u2026"
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

জটিল সংখ্যা আমাদের সংখ্যা ব্যবস্থাকে নেগেটিভ সংখ্যার বর্গমূলসমূহ অন্তর্ভুক্ত করতে সম্প্রসারিত করে, যেখানে 'কাল্পনিক' ইউনিট i হলো -1 এর বর্গমূল। প্রোগ্রামাররা তাদের ইঞ্জিনিয়ারিং, পদার্থবিজ্ঞান এবং সিগন্যাল প্রক্রিয়াকরণের মতো ক্ষেত্রে ব্যবহার করেন, কারণ তারা তরঙ্গ, দোলন এবং যা কিছু ঘোরাফেরা করে তা মডেল করার জন্য দুর্দান্ত।

## কিভাবে:

আসুন আমরা Kotlin-এ একটি বেসিক জটিল সংখ্যার ক্লাস ডিফাইন করি:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // আউটপুট: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // আউটপুট: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // আউটপুট: a * b = (-5.0 + 10.0i)
}
```

## গভীর ডুব

জটিল সংখ্যাগুলি প্রথম 16 শতকে উল্লেখিত হয়, যা বাস্তব সমাধানগুলির অভাবযুক্ত ঘনবীজ সমীকরণগুলি সমাধান করতে পরিচিত। ইঞ্জিনিয়ারিং এবং পদার্থবিজ্ঞান AC সার্কিট এবং তরঙ্গরূপ বিশ্লেষণের জন্য জটিল সংখ্যাগুলি থেকে অপার উপকার পায়। আপনি ভারী-শুল্ক কাজের জন্য Kotlin-এর `koma` বা `ejml` মতো একটি লাইব্রেরি ব্যবহার করতে পারেন।

জটিল সংখ্যার উপর অপারেশন বাস্তব সংখ্যাগুলির প্রতিফলিত হয়, কিন্তু কাল্পনিক ইউনিটের প্রতি মনোযোগ দিয়ে। উদাহরণস্বরূপ, গুন করা বন্টনীয় সম্পত্তি অনুসরণ করে, মনে রেখে যে `i^2 = -1`। এই কাল্পনিক ইউনিট আমাদেরকে বহু-মাত্রিক সংখ্যা উপস্থাপন করতে সক্ষম করে, যা বিভিন্ন বিজ্ঞানী গণনায় অপরিহার্য।

## দেখে নিন

Kotlin ম্যাথ লাইব্রেরিগুলি:

- [koma](https://koma.kyonifer.com/): Kotlin এর জন্য একটি বৈজ্ঞানিক কম্পিউটিং লাইব্রেরি।

জটিল সংখ্যা সম্পর্কে আরও পড়ুন:

- [Wikipedia: Complex Numbers](https://en.wikipedia.org/wiki/Complex_number)
