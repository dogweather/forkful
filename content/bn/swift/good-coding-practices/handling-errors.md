---
title:                "ত্রুটিগুলি পরিচালনা করা"
date:                  2024-03-17T17:51:59.069938-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
Swift-এ ত্রুটিসমূহ সম্পর্কে হ্যান্ডলিং মানে হল সমস্যাগুলি আশা করা এবং আপনার কোড চলাকালীন যে সমস্যাগুলি উপস্থিত হয় তার প্রতি সাড়া দেওয়া। আমরা এটি করি বিশৃঙ্খলা নিয়ন্ত্রণে রাখার জন্য—এপ্লিকেশনগুলি ধসে পড়া থেকে রক্ষা করে এবং ব্যবহারকারীকে সুষ্ঠু অভিজ্ঞতা দেওয়া।

## কিভাবে:
Swift `do`, `try`, এবং `catch` ব্লকের মাধ্যমে ত্রুটি হ্যান্ডলিং ব্যবহার করে। চলুন দেখি:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // ধরুন আমাদের কাছে কিছু লজিক আছে যাচাই করার জন্য যে ফাইলটি আছে কিনা এবং আমাদের এটি পড়ার অনুমতি আছে কিনা
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "ফাইলের বিষয়বস্তু এখানে যাবে"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("ওহো! ফাইলটি পাওয়া যায় নি।")
} catch FileError.noPermission {
    print("আহ! ফাইলটি পড়ার অনুমতি নেই।")
} catch {
    print("একটি অজানা ত্রুটি ঘটেছে।")
}

```

উদাহরন আউটপুট:

```
ওহো! ফাইলটি পাওয়া যায় নি।
```

## গভীর ডুব
ত্রুটি হ্যান্ডলিং সর্বদা যেমন সুন্দর ছিল না তেমনি এখন আছে। Objective-C-এ, আপনি NSError অবজেক্টের পয়েন্টারগুলির সাথে ডিল করতেন, যা বেশ ক্লান্তিকর মনে হত। এখন, আমাদের কাছে Swift এনামগুলি এবং `Error` প্রটোকল দিয়ে আরো দৃষ্টিনন্দন সিস্টেম আছে।

Swift-এর `throw` আমাদের জানান দেয় যে কিছু একটা বেসামাল হয়ে গেছে। `do` ব্লকগুলি ত্রুটি-সচেতন রিয়েলমের মতো কাজ করে, `try` প্রিফিক্স ঝুঁকিপূর্ণ ব্যাপারগুলি কল করে এবং `catch` সমস্যাগুলি সামলায় যদি তা দক্ষিণে যায়।

অপশনালগুলি সেইরূপ পরিস্থিতির জন্য একটি বিকল্প যা সরাসরি "ত্রুটি" স্থিতি নয় কিন্তু তারপরেও "কোন ফলাফল" নাও থাকতে পারে। তারা এক ধরণের শ্রোডিংগারের ভেরিয়েবলের মতো—তারা হয় একটি মান আছে না হয় নেই।

আসল গভীরতার জন্য, `Result` টাইপগুলি দেখুন, যা নিয়মিত-ফেরত এবং ত্রুটি প্যাটার্নের মধ্যে দারুণ হাইব্রিড।

## দেখুন এছাড়াও
- অফিসিয়াল Swift ত্রুটি হ্যান্ডলিং গাইড: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Swift ত্রুটি হ্যান্ডলিং সেরা অনুশীলনগুলি: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Swift-এ উন্নত ত্রুটি হ্যান্ডলিং: [Medium আর্টিকেল](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
