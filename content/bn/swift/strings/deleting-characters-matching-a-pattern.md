---
title:                "একটি নমুনা মেলে অক্ষরগুলি মুছে ফেলা"
date:                  2024-03-17T17:47:49.441043-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
