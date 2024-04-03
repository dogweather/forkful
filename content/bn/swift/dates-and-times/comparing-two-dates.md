---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:37.715177-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B8\u09C1\u0987\u09AB\u09CD\
  \u099F `Date` \u09AA\u09CD\u09B0\u0995\u09BE\u09B0\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964\
  \ \u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\u09C1\
  \u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
lastmod: '2024-03-17T18:47:44.422403-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09C1\u0987\u09AB\u09CD\u099F `Date` \u09AA\u09CD\u09B0\u0995\u09BE\
  \u09B0\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996\u09C7\u09B0 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09B9\u099C \u0989\u09AA\u09BE\u09AF\u09BC \u09B0\u09AF\u09BC\u09C7\u099B\
  \u09C7."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
সুইফ্ট `Date` প্রকারটি তারিখ এবং সময়ের জন্য ব্যবহার করে। দুটি তারিখের তুলনা করার জন্য এখানে একটি সহজ উপায় রয়েছে:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

// দুটি তারিখের অবজেক্ট তৈরি
let date1 = dateFormatter.date(from: "2023/01/01 09:00")!
let date2 = dateFormatter.date(from: "2023/02/01 10:00")!

// তারিখ তুলনা
if date1 == date2 {
    print("তারিখগুলি একই")
} else if date1 < date2 {
    print("Date1 হল Date2 এর চেয়ে আগের")
} else {
    print("Date1 হল Date2 এর চেয়ে পরের")
}
```

নমুনা আউটপুট:

`Date1 হল Date2 এর চেয়ে আগের`

`Date` `Comparable` প্রটোকল মেনে চলে বলে তুলনা অপারেটর ব্যবহার করা যায়।

## গভীর ডাইভ:
দিনের তারিখগুলি সবসময় সুবিধাজনক অবজেক্ট হিসেবে আসেনি। প্রাথমিকভাবে, বছর, মাস, এবং দিনের মতো পৃথক উপাদানগুলোর সাথে আপনাকে যুদ্ধ করতে হতো। অনেক খারাপ। এখন, সুইফ্টে `Date` অবজেক্টগুলি ভারী উত্তোলন করে এবং বিল্ট-ইন অপারেটর দ্বারা তাদের তুলনা করা সোজা।

`Date` এর আগে, অবজেক্টিভ-সি `NSDate` ব্যবহার করত, কিন্তু তারা ব্রিজেবল, তাই পুরানো কোড এখনও ভালোভাবে চলতে পারে।

এবং শুধু `<`, `>`, এবং `==` নয় — আপনি `timeIntervalSince(_:)` টি আরো সূক্ষ্ম নিয়ন্ত্রণের জন্য ব্যবহার করতে পারেন, যেমন:

```Swift
let timeInterval = date2.timeIntervalSince(date1)
```

এটি আপনাকে সেকেন্ডে পার্থক্য দেয়। পজিটিভ মান: date2 এগিয়ে আছে; নেগেটিভ: এটি পিছিয়ে আছে; শূন্য: তারা অভিন্ন। টাইমার, কাউন্টডাউন এবং সময়কাল ট্র্যাকিংয়ের জন্য খুবই কার্যকরি। অন্তরালে, তারিখগুলি কেবল সময়ের রেফারেন্স পয়েন্ট—এগুলোকে ফ্যান্সি টাইমস্ট্যাম্প হিসেবে চিন্তা করুন।

## আরো দেখুন:
- অ্যাপলের দিনের তথ্যপত্র: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- দিনের বিন্যাসন গাইড: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)
