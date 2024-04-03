---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:51.981798-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: ."
lastmod: '2024-03-17T18:47:44.410313-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কিভাবে:
```Swift
import SwiftUI

@main
struct NewProjectApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

struct ContentView: View {
    var body: some View {
        Text("Hello, new project!")
            .padding()
    }
}

// নমুনা আউটপুট:
// "Hello, new project!" টেক্সট সহ একটি উইন্ডো প্রদর্শন করে।
```

## গভীর ডাইভ
প্রি-সুইফট যুগে, Objective-C ছিল প্রধান এবং নতুন প্রকল্প শুরু করা আরও কিছুটা বেশি আমদানি করতে হত। সুইফট অবশ্য, `@main` এট্রিবিউটের মতো চমৎকার বৈশিষ্ট্য দিয়ে প্রারম্ভিক পদক্ষেপটি আরও পরিশীলিত করেছে, যা অ্যাপের প্রবেশদ্বারকে চিহ্নিত করে। Xcode-এর টেমপ্লেটগুলির মতো টুলস তুলনায়, সুইফট বিরক্তিকর কাজগুলি সহজ করে ফেলে যাতে আপনি সরাসরি মজার অংশে - আপনার ধারণাকে জীবন্ত করা - ঝাঁপ দিতে পারেন।

বিকল্পের কথা বললে, আপনি আইওএস/ম্যাকওএস অ্যাপ না বানালে একটি কমান্ড-লাইন টুল বা একটি সার্ভার-সাইড ফ্রেমওয়ার্কে যেতে পারেন। বাস্তবায়নের দিক থেকে, সুইফটের পদ্ধতিটি প্রাথমিক জটিলতা কমানোর দিকে। `ContentView` ইউআই-এর শুরুর বিন্দুকে প্রতিনিধিত্ব করে, যেখানে `WindowGroup` জানালা ব্যবস্থাপনার দায়িত্ব নেয়।

## আরও দেখুন
- [সুইফট ডকুমেন্টেশন](https://swift.org/documentation/)
- [অ্যাপলের SwiftUI টিউটোরিয়াল](https://developer.apple.com/tutorials/swiftui)
- [স্টার্ট ডেভেলপিং আইওএস অ্যাপস (সুইফট)](https://developer.apple.com/library/archive/referencelibrary/GettingStarted/DevelopiOSAppsSwift/)
