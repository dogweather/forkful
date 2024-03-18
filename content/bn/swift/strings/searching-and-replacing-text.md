---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:16:53.933487-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

প্রোগ্রামিংয়ে টেক্সট খুঁজে পেয়ে ও প্রতিস্থাপন করা ঠিক যেমনটি শোনায়: নির্দিষ্ট প্যাটার্নের জন্য স্ট্রিংগুলিকে স্ক্যান করা এবং সেগুলিকে অন্য কিছুর সাথে বদলে দেওয়া। প্রোগ্রামাররা এটি অনেক করে - ডাটা পরিষ্কার করা, ব্যবহারকারীর ইন্টারফেস আপডেট করা, অথবা স্ট্রিংগুলিকে প্রক্রিয়াজনিত করার প্রস্তুতি দেওয়া।

## কিভাবে:

```Swift
var অভিবাদন = "Hello, old friend!"

// সাধারণ প্রতিস্থাপন
অভিবাদন = অভিবাদন.replacingOccurrences(of: "old", with: "new")
print(অভিবাদন) // "Hello, new friend!"

// কেস-অনুবেদনহীন প্রতিস্থাপনের জন্য বিকল্প ব্যবহার
let caseInsensitiveResult = অভিবাদন.replacingOccurrences(
    of: "hello",
    with: "Hi",
    options: .caseInsensitive
)
print(caseInsensitiveResult) // "Hi, new friend!"

// নিয়মিত অভিব্যক্তির সাথে প্রতিস্থাপন
let regexResult = অভিবাদন.replacingOccurrences(
    of: "\\bnew\\b",
    with: "best",
    options: .regularExpression
)
print(regexResult) // "Hello, best friend!"
```

## গভীর ডুব

আমরা কম্পিউটিংয়ের প্রাথমিক দিন থেকেই স্ট্রিংয়ে টেক্সট বদলে চলেছি। শুরুতে, এটি `sed` এর মতো সাধারণ কমান্ড-লাইন টুলস দিয়ে করা হতো। Swift-এ, `replacingOccurrences(of:with:)` ভারী কাজটি করে, এবং আপনি `.caseInsensitive` অথবা `.regularExpression` এর মতো বিকল্পগুলি দ্বারা আরো নিয়ন্ত্রণ পান।

Swift-এ বিকল্পগুলির মধ্যে `NSRegularExpression` জটিল প্যাটার্নের জন্য এবং পরিবর্তনযোগ্য স্ট্রিং অপারেশনের জন্য `NSMutableString` ব্যবহার করা অন্তর্ভুক্ত। আড়ালে, Swift-এর স্ট্রিং প্রতিস্থাপন পদ্ধতিগুলি শক্তিশালী Objective-C প্রতিপক্ষগুলিতে সেতুবন্ধন করে, গতি এবং বহুমুখিতা প্রদান করে।

## দেখুন এছাড়াও

- [Swift স্ট্রিং ডকুমেন্টেশন](https://developer.apple.com/documentation/swift/string/)
- [Swift এর নিয়মিত অভিব্যক্তি](https://nshipster.com/swift-regular-expressions/)
- [Swift.org - স্ট্রিংয়ের সাথে কাজ](https://swift.org/documentation/api-design-guidelines/#strive-for-fluent-usage)
