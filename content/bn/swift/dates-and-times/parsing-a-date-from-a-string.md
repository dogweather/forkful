---
title:                "স্ট্রিং থেকে তারিখ পার্স করা"
date:                  2024-03-17T18:06:43.048602-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
একটি স্ট্রিং থেকে তারিখ পার্স করা মানে টেক্সটুয়াল তারিখ ও সময়ের প্রতিনিধিত্বকে একটি `Date` অবজেক্টে রূপান্তর করা। এই প্রক্রিয়া প্রয়োজন যেখানে তারিখগুলি স্ট্রিং হিসেবে যোগাযোগ করা হয়, যেমন API প্রতিক্রিয়া বা ব্যবহারকারীর ইনপুটে, যা তারিখের সহজ ম্যানিপুলেশন ও ফরমেটিং-এ সহায়ক হয়।

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
