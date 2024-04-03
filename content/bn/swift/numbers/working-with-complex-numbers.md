---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:57.716026-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Swift-\u098F \u099C\u099F\u09BF\
  \u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\
  \u09CB\u09A8\u09CB \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\
  \u09BF\u09A4 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u09A8\u09C7\u0987\u0964 \u09A4\
  \u09AC\u09C7 \u0986\u09AE\u09B0\u09BE \u09A8\u09BF\u099C\u09C7\u09B0\u09BE \u09A4\
  \u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09BF\
  ."
lastmod: '2024-03-17T18:47:44.402942-06:00'
model: gpt-4-0125-preview
summary: "Swift-\u098F \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\u09CB \u0985\u09A8\u09CD\u09A4\
  \u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\u09AE\u09B0\u09CD\u09A5\
  \u09A8 \u09A8\u09C7\u0987\u0964 \u09A4\u09AC\u09C7 \u0986\u09AE\u09B0\u09BE \u09A8\
  \u09BF\u099C\u09C7\u09B0\u09BE \u09A4\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09BF."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
Swift-এ জটিল সংখ্যার জন্য কোনো অন্তর্নির্মিত সমর্থন নেই। তবে আমরা নিজেরা তা তৈরি করতে পারি:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // বিয়োগ, গুণ, ইত্যাদির মতো অতিরিক্ত পদ্ধতি
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("ফলাফল: \(result.real) + \(result.imaginary)i")
// নমুনা আউটপুট: ফলাফল: 3.0 + 7.0i
```

## গভীরে গমন
১৬শ শতকে বীজগাণিতিক সমীকরণে জটিল সংখ্যাগুলি প্রকাশিত হয়েছিল। কোয়ান্টাম যান্ত্রিকি, নিয়ন্ত্রণ তত্ত্ব এবং আরও অনেক ক্ষেত্রে তারা অপরিহার্য। অ্যাপলের Swift এর জন্য কোনো মানক লাইব্রেরি নেই জটিল সংখ্যার, Python বা C++ এর মতো ভাষাগুলিতে সমর্থনের তুলনায়। নিজেদের তৈরির বিকল্পগুলির মধ্যে জটিল সংখ্যার সমর্থন সহ Numerics প্যাকেজ ব্যবহার করা বা Swift-এর সহযোগিতার সাথে C++ জটিল লাইব্রেরি মোড়ানো অন্তর্ভুক্ত থাকতে পারে।

## আরও দেখুন
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
