---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:23.814172-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Swift \u098F, \u09B8\u09CD\u099F\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0\u09C7\
  \ \u09B2\u09C7\u0996\u09BE\u099F\u09BF `FileHandle` \u0995\u09CD\u09B2\u09BE\u09B8\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B0\u09BE\
  \u09B8\u09B0\u09BF stderr \u098F\u0995\u09CD\u09B8\u09C7\u09B8\u09C7\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09B9\u099C \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09B2\u09CB."
lastmod: '2024-03-17T18:47:44.426446-06:00'
model: gpt-4-0125-preview
summary: "Swift \u098F, \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\
  \u09B0\u09CD\u09A1 \u098F\u09B0\u09B0\u09C7 \u09B2\u09C7\u0996\u09BE\u099F\u09BF\
  \ `FileHandle` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF stderr \u098F\u0995\
  \u09CD\u09B8\u09C7\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

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
