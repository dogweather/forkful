---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:39.678434-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A5\u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\
  \u09B9\u09CD\u09A8 \u09B8\u09B0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u0995\
  \u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u0998\u09BF\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09BE \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u0989\u09A6\u09CD\u09A7\u09C3\
  \u09A4\u09BF\u099A\u09BF\u09B9\u09CD\u09A8 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\
  \u09B2\u09BE\u0964 \u0986\u09AE\u09B0\u09BE \u0987\u09A8\u09AA\u09C1\u099F \u09AA\
  \u09B0\u09BF\u09B8\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\u09BE, \u09A1\u09BE\u099F\
  \u09BE \u09AE\u099C\u09C1\u09A6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\
  \u09B0\u09B8\u09CD\u09A4\u09C1\u09A4\u09BF \u0995\u09B0\u09BE, \u0985\u09A5\u09AC\
  \u09BE\u2026"
lastmod: '2024-03-17T18:47:44.396816-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\
  \u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\u09CD\
  \u09A8 \u09B8\u09B0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u0995\u09A8\u09CD\
  \u099F\u09C7\u09A8\u09CD\u099F \u0998\u09BF\u09B0\u09C7 \u09A5\u09BE\u0995\u09BE\
  \ \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\
  \u099A\u09BF\u09B9\u09CD\u09A8 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE\
  \u0964 \u0986\u09AE\u09B0\u09BE \u0987\u09A8\u09AA\u09C1\u099F \u09AA\u09B0\u09BF\
  \u09B8\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\u09BE, \u09A1\u09BE\u099F\u09BE \u09AE\
  \u099C\u09C1\u09A6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09B8\
  \u09CD\u09A4\u09C1\u09A4\u09BF \u0995\u09B0\u09BE, \u0985\u09A5\u09AC\u09BE \u09A1\
  \u09BE\u099F\u09BE \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\
  \u09B0\u09A3\u09C7 \u09AC\u09BE\u09A7\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  \ \u0985\u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C0\u09AF\u09BC \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\
  \u0982 \u09B8\u09B0\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\
  \u09BF \u0995\u09B0\u09BF\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
Swift আপনাকে উদ্ধৃতিচিহ্ন সরানোর কাজটি বেশ সুবিধাজনকভাবে করতে দেয়। এখানে একটি দ্রুত উদাহরণ দেওয়া হল `replacingOccurrences(of:with:)` ব্যবহার করে, যা ঠিক যেমনটা শোনাচ্ছে—টেক্সটের বিভিন্ন অংশ কিছু অন্য জিনিসের সাথে বিনিময় করা, অথবা একেবারেই কিছু না দিয়ে।

```swift
var quotedString = "\"This is a 'quoted' string.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // This is a 'quoted' string.

// একক উদ্ধৃতিচিহ্ন নিয়ে মোকাবেলা? শুধু অনুসন্ধানের শর্ত পরিবর্তন করুন।
quotedString = "'Here's another example.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Heres another example.
```

আউটপুট হবে উদ্ধৃতিচিহ্ন-মুক্ত স্ট্রিং, আপনি যে কাজের জন্য পরিকল্পনা করছেন সেটি জন্য সম্পূর্ণ প্রস্তুত।

## গভীর ডুব
আমরা এ ধরণের স্ট্রিং "পরিষ্কার" করে চলেছি প্রোগ্রামিং-এর সূচনালগ্ন থেকে। প্রারম্ভিক দিনগুলিতে, এটা মূলত মূল্যবান মেমরি সংরক্ষণ এবং ইনপুট প্রক্রিয়াকরণে সিনট্যাক্স ত্রুটি এড়ানোর ব্যাপার ছিল। আজকের দিনে এসে এটা JSON নিয়ে কাজ করা বা ডাটাবেইসের জন্য স্ট্রিং প্রস্তুত করার মতো ভালো ডাটা হাইজিন নিয়ে সম্পর্কিত—একটা হারানো উদ্ধৃ�িহ্ন SQL ক্যোয়ারির মতো দ্রুত "সিনট্যাক্স ত্রুটি" ঘটাতে পারে।

বিকল্প? যদি আপনি মনে করেন `replacingOccurrences(of:with:)` খুবই সাধারণ, তাহলে আপনি আরও জটিল প্যাটার্ন বা নির্দিষ্ট অবস্থানে উদ্ধৃতিচिह্ন কেবল সরাতে চাইলে, নিয়মিত এক্সপ্রেশনের দিকে ঝুঁকতে পারেন। এখানে Swift-এর `NSRegularExpression` শ্রেণী আপনার বন্ধু। তবে মনে রাখা উচিত, regex একটা দ্বি�ধারা তরবার—শক্তিশালী কিন্তু মাঝে মাঝে অতিরিক্ত।

বাস্তবায়নের দিক থেকে, `replacingOccurrences(of:with:)` Swift-এর `String` দ্বারা প্রদত্ত একটি পদ্ধতি, যা অভ্যন্তরীণভাবে ইউনিকোড এবং আধুনিক টেক্সট প্রক্রিয়াকরণের অন্যান্য জটিলতা সামলানো আরও জটিল স্ট্রিং ম্যানিপুলেশন ফাংশন ডাকে। এটি Swift-এর সেই "বাইরে থেকে সহজ, কিন্তু ভেতরে জটিল" সমাধানের একটি, যা Swift আপনার পক্ষে সম্পন্ন করে।

## আরও দেখুন
Swift-এ স্ট্রিং ম্যানিপুলেশন সম্পর্কে আরও জানতে:

- দি সুইফট প্রোগ্রামিং ল্যাঙ্গুয়েজ (স্ট্রিং এবং ক্যারেক্টারস): [Swift.org ডকুমেন্টেশন](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Apple ডেভেলপার ডকুমেন্টেশন](https://developer.apple.com/documentation/foundation/nsregularexpression)

এবং যদি আপনি এখন নিয়মিত এক্সপ্রেশন সম্পর্কে কৌতূহলী হন এবং আপনার প্যাটার্ন পরীক্ষা করতে চান:

- Regex101: [Regex টেস্টার এবং ডিবাগার](https://regex101.com)
