---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:57.716026-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\
  \u09B2\u09BF\u09B0 \u098F\u0995\u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\
  \ \u0985\u0982\u09B6 \u098F\u09AC\u0982 \u098F\u0995\u099F\u09BF \u0995\u09BE\u09B2\
  \u09CD\u09AA\u09A8\u09BF\u0995 \u0985\u0982\u09B6 \u09A5\u09BE\u0995\u09C7 (\u09AF\
  \u09C7\u09AE\u09A8 3 + 4i)\u0964 \u09B8\u09BF\u0997\u09A8\u09CD\u09AF\u09BE\u09B2\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3\
  , \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0997\u09A3\u09BF\u09A4\
  \u09C7\u09B0 \u09B8\u09AE\u09B8\u09CD\u09AF\u09BE \u09B8\u09AE\u09BE\u09A7\u09BE\
  \u09A8 \u098F\u09AC\u0982 \u09AD\u09CC\u09A4 \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\
  \u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.402942-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\
  \u09B2\u09BF\u09B0 \u098F\u0995\u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\
  \ \u0985\u0982\u09B6 \u098F\u09AC\u0982 \u098F\u0995\u099F\u09BF \u0995\u09BE\u09B2\
  \u09CD\u09AA\u09A8\u09BF\u0995 \u0985\u0982\u09B6 \u09A5\u09BE\u0995\u09C7 (\u09AF\
  \u09C7\u09AE\u09A8 3 + 4i)\u0964 \u09B8\u09BF\u0997\u09A8\u09CD\u09AF\u09BE\u09B2\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3\
  , \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0997\u09A3\u09BF\u09A4\
  \u09C7\u09B0 \u09B8\u09AE\u09B8\u09CD\u09AF\u09BE \u09B8\u09AE\u09BE\u09A7\u09BE\
  \u09A8 \u098F\u09AC\u0982 \u09AD\u09CC\u09A4 \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\
  \u09C7\u09B0 \u0985\u09A8\u09C1\u0995\u09B0\u09A3\u09C7\u09B0 \u09AE\u09A4\u09CB\
  \ \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE Swift \u09AD\u09BE\u09B7\u09BE\
  \u09AF\u09BC \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7\u0964."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কী এবং কেন?
জটিল সংখ্যাগুলির একটি বাস্তব অংশ এবং একটি কাল্পনিক অংশ থাকে (যেমন 3 + 4i)। সিগন্যাল প্রক্রিয়াকরণ, নির্দিষ্ট গণিতের সমস্যা সমাধান এবং ভৌত পদার্থের অনুকরণের মতো কাজের জন্য প্রোগ্রামাররা Swift ভাষায় তাদের ব্যবহার করে।

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
