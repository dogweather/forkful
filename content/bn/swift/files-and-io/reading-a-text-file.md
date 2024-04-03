---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:07.322658-06:00
description: "Swift \u098F \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09BF\
  \u09A4 \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7\
  \ \u09B8\u09BE\u09AE\u0997\u09CD\u09B0\u09C0 \u09AA\u09CD\u09B0\u09BE\u09AA\u09CD\
  \u09A4\u09BF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u09B2\
  \u0997 \u09AC\u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\
  \u09C0\u09B0 \u09A4\u09C8\u09B0\u09BF \u09B8\u09BE\u09AE\u0997\u09CD\u09B0\u09C0\
  \u09B0 \u09AE\u09A4\u09CB\u2026"
lastmod: '2024-03-17T18:47:44.427452-06:00'
model: gpt-4-0125-preview
summary: "Swift \u098F \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A1\
  \u09BF\u09B8\u09CD\u0995\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09BF\u09A4\
  \ \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09B8\
  \u09BE\u09AE\u0997\u09CD\u09B0\u09C0 \u09AA\u09CD\u09B0\u09BE\u09AA\u09CD\u09A4\u09BF\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u09B2\u0997 \u09AC\
  \u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0\
  \ \u09A4\u09C8\u09B0\u09BF \u09B8\u09BE\u09AE\u0997\u09CD\u09B0\u09C0\u09B0 \u09AE\
  \u09A4\u09CB \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u09A4\u09A5\u09CD\
  \u09AF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
Swift এ একটি ফাইল থেকে টেক্সট পড়তে, `String` ক্লাসের সুবিধাজনক পদ্ধতিগুলি ব্যবহার করুন। একটি ছোট উদাহরণ এখানে প্রদত্ত হল:

```Swift
import Foundation

if let filePath = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let content = try String(contentsOfFile: filePath, encoding: .utf8)
        print(content)
    } catch {
        print("ওহো! কিছু ভুল হয়েছে: \(error)")
    }
}
```
যদি "example.txt" এ "Hello, world!" থাকে, তাহলে আউটপুট হবে:
```
Hello, world!
```

## গভীরে যাচাই
প্রোগ্রামিং বিশ্বে টেক্সট ফাইল পড়া পুরানো পাহাড়ের মতই। প্রারম্ভিক সময়ে এর অর্থ ছিল পাঞ্চ কার্ড ও টেপ। এখন, Swift এর মত উচ্চ-স্তরের ভাষাগুলির সাথে, এটা সরল। উপরের স্নিপেটটি `String(contentsOfFile:)` ব্যবহার করে, কিন্তু এর বিকল্পগুলি আছে:

- `FileManager`: আরও জটিল ফাইল অপারেশনের জন্য ভাল।
- `InputStream`: বড় ফাইলগুলির সাথে কাজ করতে - কম মেমোরি-গ্রহণশীল।
- `URLSession`: রিমোট সার্ভার থেকে ফাইল আনতে।

`String(contentsOfFile:)` পদ্ধতিটি মেগা আকারের ফাইলের সাথে ব্যবহৃত হলে মেমোরি-ভারী হতে পারে। সমস্যা এড়াতে, স্ট্রিম-ভিত্তিক পদ্ধতি বা চাঙ্গা পাঠের দিকে মনোনিবেশ করুন।

## আরো দেখুন
Swift এর অফিসিয়াল ডকুমেন্টেশনে ডুব দিন:
- [String](https://developer.apple.com/documentation/swift/string)
- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [URL সেশন নিয়ে কাজ করা](https://developer.apple.com/documentation/foundation/url_loading_system/fetching_website_data_into_memory)

গভীর অনুধাবনের জন্য, এই রিসোর্সগুলি দেখুন:
- [Apple এর ফাইল সিস্টেম প্রোগ্রামিং গাইড](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
