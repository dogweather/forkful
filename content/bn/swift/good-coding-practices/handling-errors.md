---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:59.069938-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Swift `do`, `try`, \u098F\u09AC\
  \u0982 `catch` \u09AC\u09CD\u09B2\u0995\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B9\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09B2\u09BF\u0982 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7\u0964 \u099A\u09B2\u09C1\u09A8 \u09A6\u09C7\u0996\u09BF."
lastmod: '2024-03-17T18:47:44.417128-06:00'
model: gpt-4-0125-preview
summary: "Swift `do`, `try`, \u098F\u09AC\u0982 `catch` \u09AC\u09CD\u09B2\u0995\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\
  \u09BF \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09BF\u0982 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u099A\u09B2\u09C1\u09A8\
  \ \u09A6\u09C7\u0996\u09BF."
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
weight: 16
---

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
