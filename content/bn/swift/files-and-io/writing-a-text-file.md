---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:00.105586-06:00
description: "Swift \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09BF\u0996\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09AB\
  \u09BE\u0987\u09B2\u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u09A5\u09BE\u09DF\
  \u09C0\u09AD\u09BE\u09AC\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0995\
  \u09B0\u09A4\u09C7 \u09A6\u09C7\u09DF, \u09AF\u09BE \u0995\u09A8\u09AB\u09BF\u0997\
  \u09BE\u09B0\u09C7\u09B6\u09A8 \u09B8\u09C7\u099F\u09BF\u0982\u09B8, \u0987\u0989\
  \u099C\u09BE\u09B0 \u09A1\u09BE\u099F\u09BE, \u09AC\u09BE \u09B2\u0997 \u09B8\u0982\
  \u09B0\u0995\u09CD\u09B7\u09A3\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.428398-06:00'
model: gpt-4-0125-preview
summary: "Swift \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09BF\u0996\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09AB\u09BE\
  \u0987\u09B2\u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u09A5\u09BE\u09DF\u09C0\
  \u09AD\u09BE\u09AC\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\
  \u09A4\u09C7 \u09A6\u09C7\u09DF, \u09AF\u09BE \u0995\u09A8\u09AB\u09BF\u0997\u09BE\
  \u09B0\u09C7\u09B6\u09A8 \u09B8\u09C7\u099F\u09BF\u0982\u09B8, \u0987\u0989\u099C\
  \u09BE\u09B0 \u09A1\u09BE\u099F\u09BE, \u09AC\u09BE \u09B2\u0997 \u09B8\u0982\u09B0\
  \u0995\u09CD\u09B7\u09A3\u09C7\u09B0\u2026"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

Swift ব্যবহার করে টেক্সট ফাইল লিখতে পারা আপনাকে ফাইলসিস্টেমে স্ট্রিং ডাটা স্থায়ীভাবে সংরক্ষণ করতে দেয়, যা কনফিগারেশন সেটিংস, ইউজার ডাটা, বা লগ সংরক্ষণের মত কাজে অপরিহার্য। প্রোগ্রামাররা প্রায়ই এটি করে থাকে যাতে অ্যাপ লঞ্চের মধ্যে ডাটা বজায় থাকে, অ্যাপ্লিকেশনের বিভিন্ন অংশের মধ্যে ডাটা শেয়ার করা যায়, অথবা অন্যান্য প্রোগ্রাম ব্যবহারের জন্য ডাটা এক্সপোর্ট করা যায়।

## কিভাবে:

### সুইফট স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে

সুইফটের স্ট্যান্ডার্ড লাইব্রেরিতে টেক্সট ফাইল লেখার জন্য প্রয়োজনীয় সব টুল অন্তর্ভুক্ত রয়েছে। এখানে একটি মৌলিক উপায় দেওয়া হলো:

```swift
import Foundation

let content = "Hello, Wired readers! Learning Swift is fun."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/example.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("File written successfully")
} catch let error as NSError {
    print("Failed writing to URL: \(fileName), Error: " + error.localizedDescription)
}
```

এই কোড স্নিপেটটি নথিগুলি ডিরেক্টরিতে `example.txt` নামে একটি ফাইলে একটি স্ট্রিং লিখে। এটি সুইফটের ডু-ট্রাই-ক্যাচ এরর হ্যান্ডলিং ব্যবহার করে সম্ভাব্য এরর সম্বোধন করে।

### আরো নিয়ন্ত্রণের জন্য FileManager ব্যবহার করে

ফাইল এট্রিবিউটগুলির উপর আরও নিয়ন্ত্রণ বা ফাইলটি আগে থেকে আছে কি না তা চেক করার জন্য, `FileManager`‍ ব্যবহার করা যেতে পারে:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("example.txt")
    let content = "Exploring Swift for file management is enlightening."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("File already exists")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("File created and written successfully")
        } catch {
            print("Error writing file: \(error)")
        }
    }
}
```

### থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করে

Swift এর ফাইলসিস্টেম অপারেশনের জন্য জনপ্রিয় একটি থার্ড-পার্টি লাইব্রেরি হল `Files` যেটি John Sundell দ্বারা তৈরি করা হয়েছে:

প্রথমে, আপনার প্রজেক্টে Files যোগ করুন, সাধারণত Swift Package Manager এর মাধ্যমে।

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: ["Files"]),
    ]
)
```

তারপর এটি ব্যবহার করে একটি ফাইলে লিখুন:

```swift
import Files

do {
    let file = try File(path: "/path/to/your/directory/example.txt")
    try file.write(string: "Swift and Files library make a powerful combination.")
    print("File written successfully using Files library.")
} catch {
    print("An error occurred: \(error)")
}
```

`Files` লাইব্রেরির সাথে, ফাইল হ্যান্ডলিং আরও সরল হয়ে ওঠে, আপনাকে আপনার অ্যাপ্লিকেশনের ব্যবসায়িক লজিকের উপর মনোনিবেশ করতে দেয় ফাইল ম্যানেজমেন্টের জটিলতা থেকে দূরে সরিয়ে।
