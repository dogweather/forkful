---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:49.441043-06:00
description: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u0982\u099C\u09CD\u099E\u09BE\u09DF\u09BF\
  \u09A4 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u0985\u09A8\u09C1\
  \u09B8\u09BE\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09AC\u09BE \u09AF\u09A4\u09BF-\u099A\u09BF\u09B9\u09CD\u09A8 \u098F\u09B0\
  \ \u09AE\u09A4\u09CB \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\
  \u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u0985\u09A8\u09C1\u0995\u09CD\u09B0\u09AE\u2026"
lastmod: '2024-03-17T18:47:44.392005-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u0982\u099C\u09CD\u099E\u09BE\u09DF\u09BF\
  \u09A4 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u0985\u09A8\u09C1\
  \u09B8\u09BE\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09AC\u09BE \u09AF\u09A4\u09BF-\u099A\u09BF\u09B9\u09CD\u09A8 \u098F\u09B0\
  \ \u09AE\u09A4\u09CB \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\
  \u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u0985\u09A8\u09C1\u0995\u09CD\u09B0\u09AE\u2026"
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কি এবং কেন?

একটি নির্দিষ্ট প্যাটার্নের সাথে মেলে এমন অক্ষরগুলি মুছে ফেলা মানে সংজ্ঞায়িত প্যাটার্ন অনুসারে, যেমন সংখ্যা বা যতি-চিহ্ন এর মতো নির্দিষ্ট অক্ষরের অনুক্রম একটি স্ট্রিং থেকে সরিয়ে ফেলা। প্রোগ্রামাররা এটি ইনপুট পরিষ্কার করা, ডেটা পরিষ্কার করা বা প্রক্রিয়াজাত করার জন্য তৈরি করার সময় নির্দিষ্ট প্যাটার্নগুলির দরকার না থাকার জন্য এটি করে।

## কিভাবে:

```swift
import Foundation

// উদাহরণ: একটি স্ট্রিং থেকে সকল অংক সরানো
let originalString = "Contact me at 123-456-7890 after 09:00 PM."
let digitsPattern = "[0-9]"
let resultString = originalString.replacingOccurrences(of: digitsPattern, with: "", options: .regularExpression)

print(resultString)  // আউটপুট: "Contact me at -- after : PM."
```

```swift
// উদাহরণ: অ-অলফানিউমেরিক অক্ষরগুলি বাদ দেওয়া
let messyString = "H3!llo, W%@rld-"
let nonAlphanumericPattern = "[^A-Za-z0-9]"
let cleanString = messyString.replacingOccurrences(of: nonAlphanumericPattern, with: "", options: .regularExpression)

print(cleanString)  // আউটপুট: "H3lloWrld"
```

## গভীর ডুব:

সুইফ্ট এবং আধুনিক প্রোগ্রামিং এর আগে, প্যাটার্ন ম্যাচিং ছিল `sed`, `awk`, বা Perl এর মতো বিশেষ টুল এবং ভাষার একটি ক্ষেত্র যা টেক্সট প্রসেসিং ক্ষমতার জন্য পরিচিত। সুইফ্ট, এর শক্তিশালী Foundation ফ্রেমওয়ার্ক এর সাথে, ভাষায় এই কাজগুলি সহজ করে তোলে, যা বিকাশকারীদের জন্য আরও সুলভ করে তোলে। 

রেগুলার এক্সপ্রেশনকে বিকল্প হিসেবে সুইফ্টের `filter` মেথড ব্যবহার করে স্ট্রিং অতিক্রম করা যেতে পারে একটি কাস্টম শর্ত সহ, তবে এটি সময়সাপেক্ষ এবং কম পাঠ্যযোগ্য হতে পারে। রেগুলার এক্সপ্রেশন আমাদের মুছে ফেলতে বা ম্যানিপুলেট করতে চাওয়া প্যাটার্নটি বর্ণনা করার একটি সংক্ষিপ্ত, যদিও কখনও কখনও রহস্যময়, উপায় সরবরাহ করে।

ভেতরের দিকে, যখন আপনি `.regularExpression` অপশনের সাথে `replacingOccurrences(of:with:options:)` চালান, সুইফ্ট ICU রেগুলার এক্সপ্রেশন ইঞ্জিন ব্যবহার করে প্যাটার্নটি প্রক্রিয়া করে। ICU হল একটি পরিণত, ব্যাপকভাবে ব্যবহৃত লাইব্রেরি ইউনিকোড সমর্থনের জন্য, প্যাটার্ন ম্যাচিং সহ, যা অনেক উচ্চ-স্তরের প্রোগ্রামিং ভাষায় অন্তর্ভুক্ত।

## দেখুন এছাড়াও

- সুইফ্ট স্ট্রিং ডকুমেন্টেশন: https://developer.apple.com/documentation/swift/string
- সুইফ্ট রেগুলার এক্সপ্রেশন: https://developer.apple.com/documentation/foundation/nsregularexpression
- ICU ইউজার গাইড রেগুলার এক্সপ্রেশনের জন্য: https://unicode-org.github.io/icu/userguide/strings/regexp.html
