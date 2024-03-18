---
title:                "বর্তমান তারিখ পেতে"
date:                  2024-03-17T17:48:50.141823-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
Swift ভাষায় বর্তমান তারিখ পাওয়ার জন্য `Date` ক্লাসের ব্যবহার অন্তর্ভুক্ত, যেখানে app চালিত হওয়ার সময় তারিখ ও সময় অ্যাক্সেস করা যায়। প্রোগ্রামারগণ সময়ের স্ট্যাম্পিং, তারিখ হিসেব করা, ব্যবহারকারীর ইন্টারফেসে তারিখ ও সময় প্রদর্শনের মত নানাবিধ কারনে বর্তমান তারিখ আনতে প্রয়োজন পড়ে।

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
