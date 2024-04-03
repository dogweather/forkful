---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:24.981066-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09A4\u09C7, `os` \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF `Stderr` \u09AE\u09BE\u09A8\u099F\u09BF\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0\
  \ \u09AB\u09BE\u0987\u09B2\u0995\u09C7 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\
  \u09A7\u09BF\u09A4\u09CD\u09AC \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF\
  \ `fmt.Fprint`, `fmt.Fprintf`, \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.494329-06:00'
model: gpt-4-0125-preview
summary: "Go \u09A4\u09C7, `os` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF\
  \ `Stderr` \u09AE\u09BE\u09A8\u099F\u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7, \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 \u09AB\u09BE\u0987\u09B2\u0995\u09C7\
  \ \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\u09A4\u09CD\u09AC \u0995\
  \u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF `fmt.Fprint`, `fmt.Fprintf`, \u0985\u09A5\
  \u09AC\u09BE `fmt.Fprintln` \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 stderr \u098F \u09B2\
  \u09BF\u0996\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
Go তে, `os` প্যাকেজটি `Stderr` মানটি প্রদান করে, যা স্ট্যান্ডার্ড এরর ফাইলকে প্রতিনিধিত্ব করে। আপনি `fmt.Fprint`, `fmt.Fprintf`, অথবা `fmt.Fprintln` ফাংশনগুলি ব্যবহার করে stderr এ লিখতে পারেন। এখানে একটি সরল উদাহরণ দেওয়া হল:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // stderr এ একটি সরল স্ট্রিং লেখা
    _, err := fmt.Fprintln(os.Stderr, "This is an error message!")
    if err != nil {
        panic(err)
    }

    // Fprintf এর সাথে সংশোধিত এরর বার্তা
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Process completed with %d errors.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

নমুনা আউটপুট (stderr এ):
```
This is an error message!
Process completed with 4 errors.
```

মনে রাখবেন, এই বার্তাগুলি নিয়মিত আউটপুট (stdout) এ দেখা যাবে না, বরং এরর স্ট্রিমে দেখা যাবে, যা অধিকাংশ অপারেটিং সিস্টেমে পৃথকভাবে নির্দেশ করা যায়।

## গভীর ডুব:
স্ট্যান্ডার্ড এররের ধারণা ইউনিক্স দর্শনে গভীরভাবে নির্মিত, যা স্বাভাবিক আউটপুট এবং এরর বার্তাগুলির মধ্যে স্পষ্ট পার্থক্য করে, যাতে ডাটা প্রক্রিয়াকরণ এবং হ্যান্ডলিং আরও কার্যকর হয়। Go এ, এই রীতি কে `os` প্যাকেজের মাধ্যমে আলিঙ্গন করা হয়েছে, যা সরাসরি stdin, stdout, এবং stderr ফাইল ডেসক্রিপ্টর এ প্রবেশাধিকার প্রদান করে।

যদিও `os.Stderr` এ সরাসরি লেখাটি অনেক অ্যাপ্লিকেশনের জন্য যথেষ্ট, Go আরও উন্নত লগিং প্যাকেজ যেমন `log` প্রস্তাব করে, যা সময় স্ট্যাম্পিং এবং আরও নমনীয় আউটপুট কনফিগারেশনের মতো অতিরিক্ত বৈশিষ্ট্য সরবরাহ করে (যেমন, ফাইলে লেখা)। বিশেষত, বড় অ্যাপ্লিকেশনের জন্য বা যেখানে আরো ব্যাপক লগিং বৈশিষ্ট্যের প্রয়োজন হতে পারে, সেখানে `log` প্যাকেজ ব্যবহার করা একটি ভালো বিকল্প হতে পারে। মনে রাখা দরকার যে, Go এর এরর হ্যান্ডলিং এর দৃষ্টিভঙ্গি, যা ফাংশন থেকে এররগুলি ফেরত পাঠানোর উৎসাহ দেয়, stderr এ এরর বার্তা লেখার অনুশীলনকে সমর্থন করে, যা এরর ম্যানেজমেন্ট এবং রিপোর্টিং এর আরও সূক্ষ্ম নিয়ন্ত্রণ সম্ভব করে।

সারসংক্ষেপে, যদিও অনেক প্রোগ্রামিং ভাষায় stderr এ লেখা একটি মৌলিক কাজ, Go এর স্ট্যান্ডার্ড লাইব্রেরি এবং নকশা নীতিমালা এরর আউটপুট ব্যবস্থাপনার সহজ এবং উন্নত উপায় প্রস্তাব করে, যা শিল্পের ব্যাপক অনুশীলনের সাথে সামঞ্জস্য রাখে সেই সাথে Go এর বিশেষ ডিজাইন আদর্শকেও প্রতিপালন করে।
