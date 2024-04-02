---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:22.196658-06:00
description: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09AC\u09B2\u09A4\u09C7\
  \ \u09AC\u09C1\u099D\u09BE\u09DF HTML \u09AC\u09BF\u09B7\u09DF\u09AC\u09B8\u09CD\
  \u09A4\u09C1\u09B0 \u0997\u09A0\u09A8 \u09AD\u09C7\u0999\u09CD\u0997\u09C7 \u099A\
  \u09C1\u09B0\u09C7 \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09BE\u0996\u09CD\u09AF\
  \u09BE \u0995\u09B0\u09BE, \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A1\u09BE\u099F\u09BE \u0989\u09A4\
  \u09CD\u09A4\u09CB\u09B2\u09A8 \u0985\u09A5\u09AC\u09BE \u098F\u0987 \u09AC\u09BF\
  \u09B7\u09DF\u09AC\u09B8\u09CD\u09A4\u09C1\u0995\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09B0\
  \u09BF\u099A\u09BE\u09B2\u09A8\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.407148-06:00'
model: gpt-4-0125-preview
summary: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09AC\u09B2\u09A4\u09C7\
  \ \u09AC\u09C1\u099D\u09BE\u09DF HTML \u09AC\u09BF\u09B7\u09DF\u09AC\u09B8\u09CD\
  \u09A4\u09C1\u09B0 \u0997\u09A0\u09A8 \u09AD\u09C7\u0999\u09CD\u0997\u09C7 \u099A\
  \u09C1\u09B0\u09C7 \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09BE\u0996\u09CD\u09AF\
  \u09BE \u0995\u09B0\u09BE, \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A1\u09BE\u099F\u09BE \u0989\u09A4\
  \u09CD\u09A4\u09CB\u09B2\u09A8 \u0985\u09A5\u09AC\u09BE \u098F\u0987 \u09AC\u09BF\
  \u09B7\u09DF\u09AC\u09B8\u09CD\u09A4\u09C1\u0995\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09B0\
  \u09BF\u099A\u09BE\u09B2\u09A8\u09BE\u09B0\u2026"
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কী এবং কেন?
HTML পার্সিং বলতে বুঝায় HTML বিষয়বস্তুর গঠন ভেঙ্গে চুরে এবং ব্যাখ্যা করা, সাধারণত নির্দিষ্ট ডাটা উত্তোলন অথবা এই বিষয়বস্তুকে প্রোগ্রাম মাধ্যমে পরিচালনার উদ্দেশ্যে। প্রোগ্রামারগণ ওয়েব স্ক্রেপিং, ডাটা মাইনিং, স্বয়ংক্রিয় পরীক্ষা, এবং বিষয়বস্তু স্থানান্তর কাজে HTML পার্সিংয়ে লিপ্ত হন, যা অ্যাপ্লিকেশনগুলিকে ওয়েব ডকুমেন্টসের সাথে কার্যকরিভাবে মিথস্ক্রিয়া এবং প্রক্রিয়াজাত করার সুযোগ দান করে।

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
