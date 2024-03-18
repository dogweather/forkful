---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:18.558835-06:00
description: "TOML (\u099F\u09AE\u09C7\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F\
  , \u09AE\u09BF\u09A8\u09BF\u09AE\u09BE\u09B2 \u09AD\u09BE\u09B7\u09BE) \u098F\u0995\
  \u099F\u09BF \u09A1\u09BE\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\
  \u099F \u09AF\u09BE \u098F\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\
  \u09AE\u09CD\u09AF\u09BE\u09A8\u09CD\u099F\u09BF\u0995\u09CD\u09B8\u09C7\u09B0 \u0995\
  \u09BE\u09B0\u09A3\u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE TOML \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u09A8 \u0995\u09A8\u09AB\
  \u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.433416-06:00'
model: gpt-4-0125-preview
summary: "TOML (\u099F\u09AE\u09C7\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F, \u09AE\
  \u09BF\u09A8\u09BF\u09AE\u09BE\u09B2 \u09AD\u09BE\u09B7\u09BE) \u098F\u0995\u099F\
  \u09BF \u09A1\u09BE\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\
  \u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\
  \ \u09AF\u09BE \u098F\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\u09AE\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u099F\u09BF\u0995\u09CD\u09B8\u09C7\u09B0 \u0995\u09BE\
  \u09B0\u09A3\u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE TOML \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u09A8 \u0995\u09A8\u09AB\u09BF\
  \u0997\u09BE\u09B0\u09C7\u09B6\u09A8\u2026"
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?
TOML (টমের স্পষ্ট, মিনিমাল ভাষা) একটি ডাটা সিরিয়ালাইজেশন ফর্ম্যাট যা এর স্পষ্ট সিম্যান্টিক্সের কারণে পড়া সহজ। প্রোগ্রামাররা TOML ব্যবহার করেন কনফিগারেশন ফাইলে যেখানে মানুষের আদান-প্রদানের সহজতা এবং মেশিন দ্বারা সহজে পার্সিং করা গুরুত্বপূর্ণ।

## কিভাবে:
শুরু করতে, আপনার একটি TOML পার্সার প্রয়োজন। Swift-এ বিল্ট-ইন কোন পার্সার নেই, তাই `TOMLDecoder` ব্যবহার করি। Swift Package Manager এর মাধ্যমে এটি ইনস্টল করুন এবং তারপর TOML কে সহজেই সিরিয়ালাইজ এবং ডিসিরিয়ালাইজ করুন।

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML উদাহরণ"

[owner]
name = "টম প্রেস্টন-ওয়ার্নার"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("শিরোনাম: \(config.title), মালিক: \(config.owner.name), জন্মদিন: \(config.owner.dob)")
    } catch {
        print("TOML পার্সিং এ ত্রুটি: \(error)")
    }
}
```

এই কোডের আউটপুট:
```
শিরোনাম: TOML উদাহরণ, মালিক: টম প্রেস্টন-ওয়ার্নার, জন্মদিন: 1979-05-27 07:32:00 +0000
```

## গভীরে ডুব:
TOML ডিজাইন করা হয়েছিল টম প্রেস্টন-ওয়ার্নার, GitHub-এর সহ-প্রতিষ্ঠাতা দ্বারা, JSON অথবা YAML এর মতো ফর্ম্যাটের তুলনায় একটি মানুষ-বান্ধব বিকল্প হিসেবে। এর লক্ষ্য হলো স্পষ্টতা, মানুষ অথবা মেশিন দ্বারা ব্যাখ্যানের ভুল সম্ভাবনা হ্রাস করা। বিকল্প হিসাবে, YAML এবং JSON হলো সাধারণ সন্দেহভাজন, YAML মানুষের পঠনযোগ্যতার দিকে ঝুকে থাকে এবং JSON হলো সরল মেশিন-বান্ধব অপশন। Swift-এ TOML এর সাথে কাজ করতে, আমাদের কোন নেটিভ পার্সার নেই। যাইহোক, `TOMLDecoder` এর মতো তৃতীয়-পক্ষের লাইব্রেরিগুলি TOML স্ট্রিং এবং Swift টাইপের মধ্যে সহজে রূপান্তর করার সুযোগ করে দেয়, বিশেষ করে Swift 4-এ চালু করা গাড়িয়া `Codable` প্রটোকলের মাধ্যমে সিরিয়ালাইজেশনকে সরল করে।

## আরও দেখুন
- TOML স্ট্যান্ডার্ড: https://toml.io
- `TOMLDecoder` এর GitHub: https://github.com/dduan/TOMLDecoder
- `Codable` এর উপর Swift ডকুমেন্টেশন: https://developer.apple.com/documentation/swift/codable
- ডাটা সিরিয়ালাইজেশন ফর্ম্যাটের তুলনা: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
