---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:50.141823-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Swift-\u098F\u09B0 `Foundation`\
  \ \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\u09C7\
  \ `Date` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7, \u09AF\u09BE \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09AA\u09C7\u09A4\
  \u09C7 \u09B8\u09CD\u09AC\u09BE\u099A\u09CD\u099B\u09A8\u09CD\u09A6\u09CD\u09AF\
  \ \u09B9\u09DF\u09C7 \u09AF\u09BE\u09DF\u0964 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\
  \u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09DF\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995\u2026"
lastmod: '2024-03-17T18:47:44.420423-06:00'
model: gpt-4-0125-preview
summary: "Swift-\u098F\u09B0 `Foundation` \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\
  \u09BC\u09BE\u09B0\u09CD\u0995\u09C7 `Date` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE \u09AC\u09B0\u09CD\
  \u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\
  \u09AE\u09AF\u09BC \u09AA\u09C7\u09A4\u09C7 \u09B8\u09CD\u09AC\u09BE\u099A\u09CD\
  \u099B\u09A8\u09CD\u09A6\u09CD\u09AF \u09B9\u09DF\u09C7 \u09AF\u09BE\u09DF\u0964\
  \ \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\
  \u09BE\u0993\u09DF\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\
  \u0993\u09DF\u09BE \u09B9\u09B2."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
Swift-এর `Foundation` ফ্রেমওয়ার্কে `Date` ক্লাস প্রদান করে, যা বর্তমান তারিখ এবং সময় পেতে স্বাচ্ছন্দ্য হয়ে যায়। বর্তমান তারিখ পাওয়ার একটি মৌলিক উদাহরণ নিচে দেওয়া হল:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

এটি সাধারণত এরকম একটি আউটপুট প্রদান করবে:

```
2023-04-12 07:46:23 +0000
```

আউটপুট ফরম্যাটটি ISO 8601 মানের অনুসরণ করে, ইউটিসি টাইম জোনকে ব্যবহার করে। তবে, আপনি এই তারিখটি প্রদর্শনের জন্য ফরম্যাট করতে চাইতে পারেন। Swift-এর `DateFormatter` ক্লাস এখানে সাহায্য করে:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

নমুনা আউটপুট হতে পারে:

```
April 12, 2023 at 10:46:23 AM
```

ডিভাইসের লোকেল অনুসারে আউটপুট ফরম্যাট ভিন্ন হতে পারে মনে রাখবেন।

যে প্রজেক্টগুলোতে জটিল তারিখ পরিচালনা প্রয়োজন হয়, অনেক Swift ডেভেলপার তৃতীয় পক্ষের লাইব্রেরি যেমন `SwiftDate`-এর দ্বারা খোঁজে যান। নির্দিষ্ট টাইম জোন এবং ফরম্যাটে বর্তমান তারিখ পেতে `SwiftDate` ব্যবহারের একটি উপায় নিচে দেওয়া হল:

প্রথমে, SPM, CocoaPods, বা Carthage ব্যবহার করে আপনার প্রজেক্টে `SwiftDate` যোগ করুন। তারপর:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

এটি উৎপাদন করতে পারে:

```
2023-04-12 09:46:23
```

`SwiftDate` ব্যবহার করে, আপনি বিভিন্ন টাইম জোন এবং লোকেলগুলির জন্য তারিখ এবং সময় সহজেই ম্যানিপুলেট করতে পারেন, আপনার Swift অ্যাপ্লিকেশনগুলিতে জটিল তারিখ পরিচালনার কাজগুলি সহজ করে দেন।
