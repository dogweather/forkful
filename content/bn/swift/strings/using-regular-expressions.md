---
title:                "রেগুলার এক্সপ্রেশন ব্যবহার করা"
date:                  2024-03-17T18:27:37.007293-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

রেগুলার এক্সপ্রেশন, বা regex, হলো ক্যারেক্টারের একটি অনুক্রম যা একটি অনুসন্ধান প্যাটার্ন তৈরি করে, প্রায়শই স্ট্রিং মিলান বা ম্যানিপুলেশন কাজের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা তাদের ডাটা যাচাইকরণ এবং পারসিং থেকে পরিবর্তন পর্যন্ত সবকিছুর জন্য ব্যবহার করে, যা বিভিন্ন প্রোগ্রামিং ভাষা, সুইফট সহ, পাঠ্য প্রক্রিয়াজাতকরণ এবং ম্যানিপুলেশন কাজে তাদের একটি অপরিহার্য সরঞ্জামে পরিণত করে।

## কিভাবে:

সুইফটের জন্য নেটিভ সাপোর্ট রেগুলার এক্সপ্রেশন `NSRegularExpression` ক্লাস ব্যবহার করে, যা স্ট্রিং ক্লাসের রেঞ্জ এবং প্রতিস্থাপন পদ্ধতিগুলির সাথে মিলিত হয়। নিচে একটি পাঠ্য ব্লকের মধ্যে ইমেল ঠিকানা খুঁজে বের করার জন্য regex ব্যবহার করার একটি উদাহরণ দেওয়া হল:

```swift
import Foundation

let text = "Contact us at support@example.com or feedback@example.org for more information."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("পাওয়া গেছে: \(text[range])")
        }
    } else {
        print("কোনো ম্যাচ পাওয়া যায়নি।")
    }
} catch {
    print("Regex ত্রুটি: \(error.localizedDescription)")
}

// নমুনা আউটপুট:
// পাওয়া গেছে: support@example.com
// পাওয়া গেছে: feedback@example.org
```

আরও জটিল বা সুবিধা-মূলক সিনারিওর জন্য, আপনি SwiftRegex এর মতো থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করতে পারেন, যা সিনট্যাক্স সহজ করে এবং সম্ভাবনাগুলি প্রসারিত করে। যদিও সুইফটের মান লাইব্রেরি শক্তিশালী, কিছু ডেভেলপার তাদের সংক্ষিপ্ত সিনট্যাক্স এবং অতিরিক্ত বৈশিষ্ট্যের জন্য এই লাইব্রেরিগুলি পছন্দ করে। এখানে কীভাবে আপনি একটি অনুরূপ কাজ করতে পারেন একটি কল্পিত থার্ড-পার্টি লাইব্রেরি ব্যবহার করে:

```swift
// ধরে নেওয়া হয় যে একটি লাইব্রেরি SwiftRegex নামে আছে এবং আমদানি করা হয়েছে
let text = "Reach out at hello@world.com or visit our website."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // কল্পিত পদ্ধতি SwiftRegex দ্বারা প্রদান করা
if emails.isEmpty {
    print("কোনো ইমেল ঠিকানা পাওয়া যায়নি।")
} else {
    emails.forEach { email in
        print("পাওয়া গেছে: \(email)")
    }
}

// কল্পিত আউটপুট ধরে নেওয়া যায় যে `matches(for:)` পদ্ধতি SwiftRegex-এ আছে:
// পাওয়া গেছে: hello@world.com
```

এই উদাহরণটি দেখায় কীভাবে একটি তৃতীয়-পক্ষের রেগুলার এক্সপ্রেশন প্যাকেজ একটি স্ট্রিংয়ের মধ্যে মিলান খুঁজে পেতে সহজ করে তুলতে পারে, ধরে নেওয়া যায় যে এই রকম সুবিধাজনক পদ্ধতিগুলি যেমন `matches(for:)` বিদ্যমান। সঠিক সিনট্যাক্স এবং পদ্ধতির উপলব্ধতা জানতে যথাযথ তৃতীয়-পক্ষের লাইব্রেরি ডকুমেন্টেশনে পরামর্শ করা গুরুত্বপূর্ণ।
