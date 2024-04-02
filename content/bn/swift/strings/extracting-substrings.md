---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:50.491164-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\
  \u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7\
  \ \u0995\u09C7\u09AC\u09B2 \u098F\u0995\u099F\u09BF \u0985\u0982\u09B6 \u09A8\u09C7\
  \u0993\u09DF\u09BE\u2014\u09AF\u09C7\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09B0\
  \u09BF\u09AC\u09A8\u0995\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\
  \u09DF\u09CB\u099C\u09A8 \u09AE\u09A4\u09CB \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\
  \u09AF\u09C7 \u0995\u09BE\u099F\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \ \u09A5\u09BE\u0995\u09C7\u09A8 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F\u2026"
lastmod: '2024-03-17T18:47:44.397976-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\
  \u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7\
  \ \u0995\u09C7\u09AC\u09B2 \u098F\u0995\u099F\u09BF \u0985\u0982\u09B6 \u09A8\u09C7\
  \u0993\u09DF\u09BE\u2014\u09AF\u09C7\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09B0\
  \u09BF\u09AC\u09A8\u0995\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\
  \u09DF\u09CB\u099C\u09A8 \u09AE\u09A4\u09CB \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\
  \u09AF\u09C7 \u0995\u09BE\u099F\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \ \u09A5\u09BE\u0995\u09C7\u09A8 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F\u2026"
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কি এবং কেন?

সাবস্ট্রিং বের করা মানে হলো একটি স্ট্রিং থেকে কেবল একটি অংশ নেওয়া—যেমন একটি রিবনকে আপনার প্রয়োজন মতো দৈর্ঘ্যে কাটা। প্রোগ্রামাররা এটি করে থাকেন নির্দিষ্ট টেক্সট ডাটার অংশগুলি, যেমন ব্যবহারকারীর ইনপুট, ফাইলনাম, বা টেক্সট প্রসেসিং নিরীক্ষণ, বিশ্লেষণ বা ম্যানিপুলেট করতে।

## কিভাবে:

সুইফট সাবস্ট্রিং নিয়ে কাজ করা বেশ সরল করে দেয়। চলুন কিছু উদাহরণের সাথে একে ডুব দেই।

```swift
let fullString = "Hello, Swift Programmer!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 12)

// String.Index ব্যবহার করে সাবস্ট্রিং বের করা
let substring = fullString[startIndex...endIndex]

print(substring) // "Swift"

// অন্য একটি পদ্ধতি, NSRange এবং NSString ব্যবহার করে
import Foundation

let nsRange = NSRange(location: 7, length: 6)
if let range = Range(nsRange, in: fullString) {
    let substring = fullString[range]
    print(substring) // "Swift"
}

// যদি আপনি সঠিক ইন্ডেক্স জানেন, তবে দ্রুত পদ্ধতি
let quickSubstring = fullString[7...12]

print(quickSubstring) // এটি একটি ভুল দেখাবে কারণ সুইফট স্ট্রিংগুলি ইন্টিজার ইন্ডেক্সিং সমর্থন করে না
```

আউটপুট:
```
Swift
Swift
// ভুল: 'subscript(_:)' উপলব্ধ নেই: cannot subscript String with an Int, আরও তথ্যের জন্য String ডকুমেন্টেশন দেখুন
```

## বিস্তারিত বিবেচনা

সুইফটে সাবস্ট্রিং বের করা মানে হলো সুইফট যেভাবে স্ট্রিং হ্যান্ডেল করে, তা বুঝতে হবে, যা Python বা C# এর মতো ভাষাগুলো থেকে আলাদা। সুইফটে, স্ট্রিংগুলি ক্যারাক্টারের সংগ্রহ, যা ইন্টিজার ইন্ডেক্সগুলি ব্যবহার করে না। এটি ইউনিকোড-সামঞ্জস্যপূর্ণ ক্যারাক্টারগুলির জন্য সুইফটের সমর্থন থেকে উদ্ভূত, যা স্ট্রিংগুলিকে নির্দিষ্ট দৈর্ঘ্যের নয়, বরং গ্রাফেম ক্লাস্টারের একটি সংগ্রহ করে তোলে (যা একজন ব্যবহারকারী একটি একক ক্যারাক্টার হিসেবে উপলব্ধি করে)।

এই ডিজাইন মানে সরাসরি ইন্টিজার সাবস্ক্রিপ্টিং সুইফট স্ট্রিংগুলির সাথে কাজ করবে না; আপনাকে `String.Index` দিয়ে কাজ করতে হবে। যদিও এটি ইন্টিজার ব্যবহারের মতো সাথে সাথে সহজবোধ্য নয়, এটি বিভিন্ন টেক্সট স্ক্রিপ্ট এবং ইমোজিকে ধারাবাহিকভাবে হ্যান্ডেল করে।

বিকল্প হিসেবে Objective-C থেকে `NSString` ব্যবহার করা যায়, যেমন উদাহরণগুলিতে দেখানো হয়েছে, যা NSRange-এর জন্য অনুমতি দেয়, কিন্তু এটি ধরণের পুরাতন এবং সুইফটি নয়। সুইফট 4 থেকে, String নিজেই অনেক ভালোবাসা পেয়েছে, সাবস্ট্রিং নিয়ে কাজ করার জন্য ধনী, আরও স্বজ্ঞানুগ এপিআই অপশনগুলির সাথে, বেশিরভাগ কাজের জন্য `NSString` কে পিছনে ফেলে দিয়েছে।

বাস্তবায়নের বিস্তারিত জিনিসগুলি অত্যন্ত গুরুত্বপূর্ণ—সারল সাবস্ট্রিং এক্সট্রাকশন ইউনিকোড-সামঞ্জস্যপূর্ণ স্ট্রিংগুলির সাথে ডিল করার সময় পারফরমেন্স হিট ঘটাতে পারে কারণ প্রতিটি `index(_: offsetBy:)` কল O(n) হতে পারে। এছাড়াও, যখন আপনি সুইফটে একটি সাবস্ট্রিং তৈরি করেন, এটি মূল স্ট্রিংয়ের মেমরি শেয়ার করে, যা দক্ষ, কিন্তু পরে যদি আপনি মূল স্ট্রিংটি পরিবর্তন করেন তবে এটি সচেতন থাকার জিনিস।

## আরও দেখুন

এই বিষয়ে আরও জানতে অফিসিয়াল ডকসগুলি দেখুন:

- সুইফট স্ট্রিং এবং ক্যারেক্টার: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- স্ট্রিং প্রোগ্রামিং গাইড: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/)

অনুশীলন ও সুইফট প্লেগ্রাউন্ডে খেলার মাধ্যমে হাত নোংরা করুন এবং এটির সঠিক অনুভূতি পান।
