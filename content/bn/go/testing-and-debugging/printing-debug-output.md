---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:32.617624-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0997\u09CB \u09A4\u09C7, \u0986\
  \u09AA\u09A8\u09BF \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A1\u09BF\u09AC\u09BE\
  \u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\
  \u099F \u0995\u09B0\u09A4\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09A1\u09BE\u09B0\u09CD\u09A1 `fmt` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8\u0964 `fmt` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\
  \u09BF \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09A7\u09B0\u09A8\u09C7\u09B0\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\
  \u09C7,\u2026"
lastmod: '2024-03-17T18:47:43.478410-06:00'
model: gpt-4-0125-preview
summary: "\u0997\u09CB \u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u0995\u09A8\u09B8\u09CB\
  \u09B2\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09A4\u09C7 \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 `fmt` \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 `fmt` \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8\
  \ \u09A7\u09B0\u09A8\u09C7\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 `Println`, `Printf`,\
  \ \u098F\u09AC\u0982 `Print`, \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AB\u09B0\
  \u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0982 \u099A\u09BE\u09B9\u09BF\u09A6\u09BE\
  \ \u09B8\u09BE\u09AA\u09C7\u0995\u09CD\u09B7\u09C7\u0964."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
গো তে, আপনি কনসোলে ডিবাগ আউটপুট প্রিন্ট করতে স্ট্যান্ডার্ড `fmt` প্যাকেজটি ব্যবহার করতে পারেন। `fmt` প্যাকেজটি বিভিন্ন ধরনের ফাংশন প্রদান করে, যেমন `Println`, `Printf`, এবং `Print`, বিভিন্ন ফরম্যাটিং চাহিদা সাপেক্ষে।

```go
package main

import (
    "fmt"
)

func main() {
    // সাধারণ বার্তা
    fmt.Println("Debug: Entering main function")

    var name = "Gopher"
    // ফরম্যাটেড বার্তা
    fmt.Printf("Hello, %s! This is a debug message.\n", name)

    // fmt.Print ব্যবহার করে
    debugMsg := "This is another debug message."
    fmt.Print("Debug: ", debugMsg, "\n")
}
```

নমুনা আউটপুট:
```
Debug: Entering main function
Hello, Gopher! This is a debug message.
Debug: This is another debug message.
```

আরও উন্নত ডিবাগিং এর জন্য, গো'র `log` প্যাকেজ টাইমস্ট্যাম্পস অন্তর্ভুক্ত করে এবং শুধু কনসোলে নয়, বিভিন্ন গন্তব্যে আউটপুট দিতে ব্যবহৃত হতে পারে।

```go
package main

import (
    "log"
    "os"
)

func main() {
    // একটি লগ ফাইল তৈরি
    file, err := os.OpenFile("debug.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
    if err != nil {
        log.Fatal("Error creating log file:", err)
    }
    defer file.Close()

    // লগের আউটপুট ফাইলে সেট করা
    log.SetOutput(file)

    log.Println("This is a debug message with a timestamp.")
}
```

`debug.log` এ বার্তাটি এমন দেখাবে:
```
2023/04/01 15:00:00 This is a debug message with a timestamp.
```

## গভীর ডুব
কম্পিউটার প্রোগ্রামিংয়ে ডিবাগ আউটপুট প্রিন্টিং একটি দীর্ঘস্থায়ী অনুশীলন, যার বাস্তবায়ন বিভিন্ন ভাষায় ভিন্ন হয়। গোতে, স্ট্যান্ডার্ড লাইব্রেরির `fmt` এবং `log` প্যাকেজগুলি সোজাসাপটা এবং বহুমুখী বিকল্প সরবরাহ করে। যদিও `fmt` প্যাকেজ মৌলিক ডিবাগিং চাহিদা পূরণ করে, `log` প্যাকেজ লগিং লেভেল এবং কনফিগারেবল আউটপুট গন্তব্য যেমন উন্নত ফাংশনালিটি সরবরাহ করে।

তদুপরি, যেমন অ্যাপ্লিকেশনগুলি আরও জটিল হয়, `zap` এবং `logrus` এর মতো লগিং ফ্রেমওয়ার্কগুলি গঠনমূলক লগিং এবং উন্নত পারফরম্যান্সের মতো আরও উন্নত বৈশিষ্ট্য সরবরাহ করতে পারে। এই তৃতীয়-পক্ষের প্যাকেজগুলি ডেভেলপারদের তাদের লগিং কৌশলকে তাদের নির্দিষ্ট চাহিদা অনুযায়ী সাজাতে সহায়তা করে।

তবে, লগিংয়ে সঠিক ভারসাম্য অবশ্যই অবলম্বন করা উচিত। অতিরিক্ত ডিবাগ আউটপুট লগগুলিকে জটিল করে তুলতে পারে এবং উপকারী তথ্য খুঁজে পেতে কঠিন করে তুলতে পারে। ডেভেলপারদের বিভিন্ন লগ লেভেল (যেমন, ডিবাগ, ইনফো, ওয়ার্ন, এরর) ব্যবহার করে বার্তাগুলির গুরুত্ব বিভাগ করা উচিত, যাতে লগগুলি নেভিগেট করা সহজ এবং অর্থপূর্ণ হয়।
