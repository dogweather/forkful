---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:31.434804-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Swift `DateFormatter` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 `Date` \u0985\u09AC\u099C\u09C7\
  \u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AA\u09A1\u09BC\u09A4\u09C7\
  \ \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\u099C\u09A8\u0995 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u09AF\u09BC\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\
  \u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  ."
lastmod: '2024-03-17T18:47:44.421434-06:00'
model: gpt-4-0125-preview
summary: "Swift `DateFormatter` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 `Date` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\
  \u0995\u09C7 \u09AA\u09A1\u09BC\u09A4\u09C7 \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\
  \u099C\u09A8\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7\
  \ \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
Swift `DateFormatter` ব্যবহার করে `Date` অবজেক্টগুলিকে পড়তে সুবিধাজনক স্ট্রিংয়ে পরিণত করে। এখানে কিভাবে:

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: date)
print(dateString) // আউটপুট: "2023-04-05 14:20:35" (অথবা বর্তমান তারিখ এবং সময়)
```

আপনার তারিখ কেমন দেখাবে তা পরিবর্তন করতে `dateFormat` পরিবর্তন করুন:

```Swift
formatter.dateFormat = "EEEE, MMM d, yyyy"
print(formatter.string(from: date)) // আউটপুট: "Wednesday, Apr 5, 2023"
```

## গভীর ডাইভ
`DateFormatter`-এর আগে, Objective-C এবং প্রাথমিক Swift ব্যবহার করেছিল `NSDateFormatter`, যা মূলত একই জিনিস পুনঃব্র্যান্ডেড। মূল বিষয়টি হল আইএসও 8601 জানা, যা একটি সাধারণ তারিখ ফরম্যাট মান। ডেভেলপারদের কাস্টম ফর্ম্যাট এবং ব্যবহারকারীর লোকেল সেটিংসের মধ্যে সামঞ্জস্য রাখতে হবে। কেন? সারা বিশ্বে তারিখগুলি ভিন্নভাবে পড়া হয়। উদাহরণস্বরূপ, আমেরিকানরা "MM/dd/yyyy" ব্যবহার করে, যেখানে অনেক ইউরোপীয় দেশ "dd/MM/yyyy" ব্যবহার করে।

বিকল্প? নিশ্চয়ই। Swift আইএসও 8601 তারিখগুলির জন্য `ISO8601DateFormatter` এবং সময় অবধিগুলির জন্য `DateComponentsFormatter` যেমন "42 মিনিট" প্রস্তাব করে। আপনি Swift 5.5 এর আগেও কাস্টম বেছে নিতে পারেন `.formatted()` এর মাধ্যমে:

```Swift
let formattedDate = date.formatted(.dateTime.year().month().day().hour().minute().second())
print(formattedDate) // আউটপুট আপনার লোকেল সেটিংসের উপর নির্ভর করবে
```

সতর্কতা: কাস্টম স্ট্রিং তৈরি করা স্থানীয় ভাষা সম্পর্কিত সমস্যা এবং ভুল প্রবণ কোডে প্রবেশ করাতে পারে। সম্ভাব্য হলে ফর্ম্যাটার এবং মানদণ্ডগুলি সাথে থাকুন।

## আরো দেখুন
- [তারিখ ফরম্যাটিং](https://developer.apple.com/documentation/foundation/dateformatter) - DateFormatter সম্পর্কে অ্যাপলের ডকুমেন্টেশন
