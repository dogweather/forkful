---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:22.196658-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B8\u09C1\u0987\u09AB\u099F\
  \ \u09A1\u09BF\u09AB\u09B2\u09CD\u099F \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 HTML\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982\u09DF\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u0995\u09CB\u09A8\u09CB \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\
  \u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u09A8\u09BE, \u098F\u0987 \u0995\
  \u09BE\u099C \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09B0\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09A4\u09C3\u09A4\u09C0\u09DF-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u2026"
lastmod: '2024-04-05T21:53:53.013804-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09C1\u0987\u09AB\u099F \u09A1\u09BF\u09AB\u09B2\u09CD\u099F \u09B9\
  \u09BF\u09B8\u09C7\u09AC\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982\u09DF\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\u09CB \u09A8\u09BF\u09B0\
  \u09CD\u09AE\u09BF\u09A4 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\
  \u09C7 \u09A8\u09BE, \u098F\u0987 \u0995\u09BE\u099C \u0995\u09BE\u09B0\u09CD\u09AF\
  \u0995\u09B0\u09BF\u09AD\u09BE\u09AC\u09C7 \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A4\u09C3\u09A4\u09C0\u09DF-\u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u09AA\u09CD\u09B0\u09DF\
  \u09CB\u099C\u09A8 \u0995\u09B0\u09C7\u0964 SwiftSoup \u09B9\u09B2 \u098F\u0995\u099F\
  \u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09DF \u09AA\u099B\u09A8\u09CD\u09A6\
  , \u098F\u0995\u099F\u09BF \u09AC\u09BF\u09B6\u09C1\u09A6\u09CD\u09A7 Swift \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AF\u09BE HTML \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\
  \u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF jQuery-\u098F\
  \u09B0 \u09AE\u09A4\u09CB \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\u09CD\
  \u09B8 \u09AA\u09CD\u09B0\u09B8\u09CD\u09A4\u09BE\u09AC \u0995\u09B0\u09C7\u0964\
  ."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
সুইফট ডিফল্ট হিসেবে HTML পার্সিংয়ের জন্য কোনো নির্মিত লাইব্রেরি অন্তর্ভুক্ত করে না, এই কাজ কার্যকরিভাবে সম্পাদনের জন্য তৃতীয়-পক্ষের লাইব্রেরি ব্যবহারের প্রয়োজন করে। SwiftSoup হল একটি জনপ্রিয় পছন্দ, একটি বিশুদ্ধ Swift লাইব্রেরি যা HTML পার্সিং এবং ম্যানিপুলেশনের জন্য jQuery-এর মতো সিনট্যাক্স প্রস্তাব করে।

### ইনস্টলেশন
প্রথমে, আপনার প্রকল্পে SwiftSoup যোগ করতে হবে। যদি আপনি Swift Package Manager ব্যবহার করেন, তাহলে আপনার `Package.swift` নির্ভরতাগুলির মধ্যে এটি যোগ করতে পারেন:

```swift
dependencies: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
]
```

### উদাহরণ: HTML থেকে লিঙ্ক উত্তোলন
ধরুন আপনার কাছে একটি HTML ডকুমেন্ট রয়েছে এবং আপনি সকল লিঙ্ক (`<a href="...">`) উত্তোলন করতে চান। SwiftSoup ব্যবহার করে, আপনি সহজেই এটি সম্পন্ন করতে পারেন:

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>স্যাম্পল পেজ</title>
</head>
<body>
    <p>আমাদের ওয়েবসাইটে স্বাগতম</p>
    <a href="https://example.com/page1">পেজ 1</a>
    <a href="https://example.com/page2">পেজ 2</a>
</body>
</html>
"""

do {
    let doc: Document = try SwiftSoup.parse(html)
    let links: Elements = try doc.select("a")
    for link in links.array() {
        let linkHref: String = try link.attr("href")
        let linkText: String = try link.text()
        print("\(linkText) - \(linkHref)")
    }
} catch Exception.Error(let type, let message) {
    print("ভুলের ধরন: \(type) বার্তা: \(message)")
} catch {
    print("ভুল")
}
```

### নমুনা আউটপুট
পূর্ববর্তী কোডটি HTML থেকে URL এবং তাদের টেক্সট উত্তোলন করে, আউটপুট হয়:

```
পেজ 1 - https://example.com/page1
পেজ 2 - https://example.com/page2
```

এই মৌলিক উদাহরণটি দেখায় কিভাবে SwiftSoup ব্যবহার করে HTML ডকুমেন্টস পার্স করা যায়। SwiftSoup-এর ডকুমেন্টেশন আরও বিস্তারিত অন্বেষণ করে, আপনি HTML বিষয়বস্তু নেভিগেট, অনুসন্ধান এবং মডিফাই করার বহুবিধ পদ্ধতি খুঁজে পেতে পারেন, যা আপনার Swift অ্যাপ্লিকেশনগুলিকে জটিল ওয়েব বিষয়বস্তু সহজে প্রক্রিয়াজাত করার ক্ষমতা দান করে।
