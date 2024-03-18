---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:23.814172-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09C7\u09B0 \u098F\u09B0\u09B0 \u09AE\u09C7\u09B8\u09C7\u099C \u09AC\
  \u09BE \u09A1\u09BE\u09AF\u09BC\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\
  \u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u0986\u09B2\u09BE\u09A6\
  \u09BE \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:44.426446-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09C7\u09B0 \u098F\u09B0\u09B0 \u09AE\u09C7\u09B8\u09C7\u099C \u09AC\
  \u09BE \u09A1\u09BE\u09AF\u09BC\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\
  \u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u0986\u09B2\u09BE\u09A6\
  \u09BE \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7\
  \u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
---

{{< edit_this_page >}}

## কি এবং কেন?

স্ট্যান্ডার্ড এরর (stderr) এ লেখা মানে আপনার প্রোগ্রামের এরর মেসেজ বা ডায়াগনস্টিক আউটপুটগুলোকে স্ট্যান্ডার্ড আউটপুট (stdout) থেকে আলাদা একটি স্ট্রিমে পরিচালনা করা। এটি ডিবাগিং এবং এররগুলোকে লগ করা স্ট্যান্ডার্ড আউটপুট স্পষ্ট রাখার জন্য অত্যন্ত জরুরি, যা ডেভেলপার এবং ব্যবহারকারীদের প্রোগ্রামের অবস্থা এবং সমস্যাগুলি বোঝার ক্ষেত্রে সহায়ক।

## কিভাবে:

Swift এ, স্ট্যান্ডার্ড এররে লেখাটি `FileHandle` ক্লাস ব্যবহার করে সরাসরি stderr এক্সেসের মাধ্যমে করা যেতে পারে। এখানে একটি সহজ উদাহরণ দেওয়া হলো:

```swift
import Foundation

// একটি মেসেজ নির্ধারণ করা
let errorMessage = "An error occurred.\n"

// মেসেজটিকে ডাটায় রূপান্তর করা
if let data = errorMessage.data(using: .utf8) {
    // এরর মেসেজটিকে stderr এ লেখা
    FileHandle.standardError.write(data)
}
```

stderr এ আউটপুট (সাধারণত একটি কনসোল বা টার্মিনালে দেখা যায়):
```
An error occurred.
```

আরো জটিল লগিং বা বাহ্যিক লাইব্রেরিগুলির সাথে কাজ করার ক্ষেত্রে, কেউ তৃতীয়-পক্ষের লাইব্রেরি যেমন **SwiftLog** ব্যবহার করা বিবেচনা করতে পারেন। যদিও **SwiftLog** সরাসরি বাক্সের বাইরে stderr এ লেখে না, আপনি একটি কাস্টম লগিং ব্যাকএন্ড বাস্তবায়ন করে এটি অর্জন করতে পারেন। এখানে stderr এ লেখার জন্য একটি কাস্টম লগ হ্যান্ডলার নির্ধারণের একটি সরলীকৃত উদাহরণ দেওয়া হলো:

প্রথমে, `Package.swift` এ আপনার প্রকল্পের নির্ভরতা হিসাবে **SwiftLog** যোগ করুন:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

তারপর, stderr এ লেখার জন্য একটি কাস্টম লগ হ্যান্ডলার বাস্তবায়ন করুন:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// ব্যবহার
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("This is an error message")
```

stderr এ আউটপুট:
```
This is an error message
```

এই কাস্টম হ্যান্ডলারটি আপনাকে আপনার SwiftLog এরর মেসেজগুলিকে সরাসরি স্ট্যান্ডার্ড এররে প্রেরণ করতে সহায়তা করে, আপনার অ্যাপ্লিকেশন অন্যান্য লগ মেসেজগুলির সাথে সহজে ইন্টিগ্রেট করে।
