---
title:                "টমল নিয়ে কাজ করা"
date:                  2024-03-17T18:31:18.558835-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
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
