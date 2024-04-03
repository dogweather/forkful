---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:54:22.259487-06:00
description: "\u09B2\u0997\u09BF\u0982 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0985\
  \u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u0986\
  \u099A\u09B0\u09A3, \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u098F\u09AC\u0982 \u0985\
  \u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u0997\u09C1\u09B0\u09C1\u09A4\u09CD\u09AC\
  \u09AA\u09C2\u09B0\u09CD\u09A3 \u09A4\u09A5\u09CD\u09AF \u09B8\u099E\u09CD\u099A\
  \u09BF\u09A4 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B0\u09C7\u0995\u09B0\
  \u09CD\u09A1 \u0995\u09B0\u09BE, \u09AF\u09C7\u09AE\u09A8 \u098F\u0995\u099F\u09BF\
  \ \u09AB\u09BE\u0987\u09B2 \u09AC\u09BE \u09A1\u09BE\u099F\u09BE\u09AC\u09C7\u09B8\
  \u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u09A4\u09BE\u09A6\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.416145-06:00'
model: gpt-4-0125-preview
summary: "\u09B2\u0997\u09BF\u0982 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0985\u09CD\
  \u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u0986\u099A\
  \u09B0\u09A3, \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u098F\u09AC\u0982 \u0985\u09A8\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u0997\u09C1\u09B0\u09C1\u09A4\u09CD\u09AC\u09AA\
  \u09C2\u09B0\u09CD\u09A3 \u09A4\u09A5\u09CD\u09AF \u09B8\u099E\u09CD\u099A\u09BF\
  \u09A4 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B0\u09C7\u0995\u09B0\u09CD\
  \u09A1 \u0995\u09B0\u09BE, \u09AF\u09C7\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\
  \u09BE\u0987\u09B2 \u09AC\u09BE \u09A1\u09BE\u099F\u09BE\u09AC\u09C7\u09B8\u09C7\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B8\u09C7\
  \u09B0 \u09B8\u09CD\u09AC\u09BE\u09B8\u09CD\u09A5\u09CD\u09AF \u098F\u09AC\u0982\
  \ \u09AA\u09BE\u09B0\u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u09A8\u09CD\u09B8 \u099F\
  \u09CD\u09B0\u09CD\u09AF\u09BE\u0995 \u0995\u09B0\u09A4\u09C7, \u09B8\u09AE\u09B8\
  \u09CD\u09AF\u09BE \u09A1\u09BF\u09AC\u09BE\u0997 \u0995\u09B0\u09A4\u09C7, \u098F\
  \u09AC\u0982 \u0989\u09CE\u09AA\u09BE\u09A6\u09A8 \u09AA\u09B0\u09BF\u09AC\u09C7\
  \u09B6\u09C7\u09B0 \u0986\u09A1\u09BC\u09BE\u09B2\u09C7 \u0995\u09BF \u0998\u099F\
  \u099B\u09C7 \u09A4\u09BE \u09A8\u099C\u09B0\u09C7 \u09B0\u09BE\u0996\u09A4\u09C7\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u0964."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কিভাবে:
সুইফ্টে, আপনি কনসোলে লগ লিখতে পারেন প্রিন্ট বিবৃতি ব্যবহার করে অথবা আরও লচ্ছেদার `os.log` API ব্যবহার করে, যা অ্যাপেল প্ল্যাটফর্মে ইউনিফাইড লগিং সিস্টেমে আঁকড়ে থাকে।

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // সাধারণ প্রিন্ট বিবৃতি
    print("Fetch started")
    
    // os.log ব্যবহার করে ইনফো-লেভেল ইভেন্ট লগিং  
    os_log(.info, log: logger, "API থেকে ডাটা আনা হচ্ছে।")
    
    do {
        let data = try performNetworkRequest()
        // ডিবাগ-লেভেল ইভেন্ট লগিং
        os_log(.debug, log: logger, "ডাটা প্রাপ্ত: %@", data.description)
    } catch {
        // এরর-লেভেল ইভেন্ট লগিং
        os_log(.error, log: logger, "ডাটা আনতে ব্যর্থ: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // নেটওয়ার্ক অনুরোধ অনুকরণ করুন
    return Data()
}
```

কনসোলে নমুনা আউটপুট এরকম দেখাবে:

```
Fetch started
API থেকে ডাটা আনা হচ্ছে।
ডাটা প্রাপ্ত: কিছু ডাটার বাইট...
```

ত্রুটির জন্য, এটি হতে পারে:

```
ডাটা আনতে ব্যর্থ: ইন্টারনেট সংযোগ অফলাইন মনে হচ্ছে।
```

## গভীর ডুব
আইওএস ১০ এবং ম্যাকওএস সিয়েরাতে চালু হওয়া ইউনিফাইড লগিং সিস্টেমের সাথে সুইফ্টে লগিং নতুন শক্তি এবং দক্ষতা লাভ করে। সরাসরি কনসোলে যাওয়া `প্রিন্ট` বিবৃতির বিপরীতে, এই সিস্টেমটি কার্যকলাপ-ভিত্তিক, এবং আপনাকে তাদের গুরুত্ব এবং তারা ডিবাগ বা রিলিজ বিল্ড হলে তার ভিত্তিতে লগ বার্তা ফিল্টার করতে দেয়।

ঐতিহাসিক প্রেক্ষাপট আইওএস এবং ম্যাকওএসে লগিংয়ের বিবর্তনকে চিত্রিত করে, সরল প্রিন্ট বিবৃতিগুলি থেকে যন্ত্রগুলির অ্যাপ্লিকেশন এবং কনসোলের সাথে একীভূত জটিল টুলগুলির দিকে।

সুইফ্টের মধ্যে লগিংয়ের বিভিন্ন বিকল্প রয়েছে, যেমন কোকোআলাম্বারজ্যাকের মতো থার্ড-পার্টি লাইব্রেরিগুলি, যা ইউনিফাইড লগিং সিস্টেমের উপর একটি ম্যাক্রো স্তর অফার করে। এটি লগ বিন্যাস, ফাইল ব্যবস্থাপনা, এবং পারফরম্যান্স বিকল্�
