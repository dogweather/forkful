---
title:                "তারিখকে স্ট্রিং এ রূপান্তর করা"
date:                  2024-03-17T17:46:31.434804-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
Swift এ একটি তারিখকে স্ট্রিংয়ে রূপান্তর করা মানুষের জন্য তারিখ ফরম্যাট করতে দেয়। এটি UI ডিসপ্লে, লগিং বা যখনই মানুষের কাছে তারিখগুলি অর্থপূর্ণ করা প্রয়োজন, শুধুমাত্র কোড নয়, তখন এটি মূল বিষয়।

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
