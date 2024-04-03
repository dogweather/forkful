---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:43.048602-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\
  \u09B8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F\u09C1\u09AF\u09BC\u09BE\u09B2 \u09A4\u09BE\u09B0\u09BF\u0996 \u0993 \u09B8\
  \u09AE\u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\
  \u09A4\u09CD\u09AC\u0995\u09C7 \u098F\u0995\u099F\u09BF `Date` \u0985\u09AC\u099C\
  \u09C7\u0995\u09CD\u099F\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u0987 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\
  \u09AF\u09BC\u09BE \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09AF\u09C7\
  \u0996\u09BE\u09A8\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.419334-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\
  \u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \u09C1\u09AF\u09BC\u09BE\u09B2 \u09A4\u09BE\u09B0\u09BF\u0996 \u0993 \u09B8\u09AE\
  \u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\u09A4\
  \u09CD\u09AC\u0995\u09C7 \u098F\u0995\u099F\u09BF `Date` \u0985\u09AC\u099C\u09C7\
  \u0995\u09CD\u099F\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\
  \u09B0\u09BE\u0964 \u098F\u0987 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\
  \u09BC\u09BE \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09AF\u09C7\u0996\
  \u09BE\u09A8\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u09AF\
  \u09CB\u0997\u09BE\u09AF\u09CB\u0997 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC, \u09AF\
  \u09C7\u09AE\u09A8 API \u09AA\u09CD\u09B0\u09A4\u09BF\u0995\u09CD\u09B0\u09BF\u09AF\
  \u09BC\u09BE \u09AC\u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\
  \u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F\u09C7, \u09AF\u09BE \u09A4\u09BE\
  \u09B0\u09BF\u0996\u09C7\u09B0 \u09B8\u09B9\u099C \u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8 \u0993 \u09AB\u09B0\u09AE\u09C7\u099F\
  \u09BF\u0982-\u098F \u09B8\u09B9\u09BE\u09AF\u09BC\u0995 \u09B9\u09AF\u09BC\u0964\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:


### Foundation's `DateFormatter` ব্যবহার করে
Swift-এর স্ট্যান্ডার্ড লাইব্রেরি, Foundation, স্ট্রিংগুলিকে `Date` অবজেক্টে এবং উল্টোভাবে রূপান্তর করার জন্য `DateFormatter` প্রদান করে। একটি স্ট্রিং থেকে তারিখ পার্স করতে, আপনাকে মেলে যাওয়া দিনের ফরমেটটি নির্দিষ্ট করতে হবে, তারপর ফরমেটারটি ব্যবহার করে এটি পার্স করতে হবে।

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("Parsed date: \(date)")
} else {
    print("Failed to parse date")
}
// নমুনা আউটপুট: Parsed date: 2023-04-29 22:00:00 +0000
```

লক্ষ্য করুন যে আউটপুট আপনার টাইমজোন ভিত্তিক ভিন্ন হতে পারে।

### ISO8601DateFormatter ব্যবহার করে
ISO 8601 দিনের ফরমেটের জন্য, Swift একটি বিশেষায়িত ফরমেটার, `ISO8601DateFormatter` প্রদান করে, যা পার্সিং প্রক্রিয়াটি সহজ করে দেয়।

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("Parsed ISO8601 date: \(date)")
} else {
    print("Failed to parse ISO8601 date")
}
// নমুনা আউটপুট: Parsed ISO8601 date: 2023-04-30 15:00:00 +0000
```

### একটি থার্ড-পার্টি লাইব্রেরি ব্যবহার করে: SwiftDate
যদিও Swift তারিখ পার্স করার জন্য দৃঢ় টুলস সরবরাহ করে, তৃতীয়-পক্ষের লাইব্রেরিগুলি যেমন SwiftDate আরও বেশি নমনীয়তা এবং সুবিধা প্রদান করে। আপনার প্রকল্পে SwiftDate যোগ করার পর, পার্সিং যেমন সহজ হয়ে ওঠে:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("Parsed date with SwiftDate: \(date)")
} else {
    print("Failed to parse date with SwiftDate")
}
// নমুনা আউটপুট: Parsed date with SwiftDate: 2023-04-30 00:00:00 +0000
```

SwiftDate প্রাকৃতিক ভাষা এবং বিস্তৃত পরিসরের দিনের ফরমেট সহ পার্সিং সহজ করে তোলে, যা এটিকে আপনার Swift প্রোগ্রামিং টুলকিটের শক্তিশালী একটি যোগান করে তোলে।
