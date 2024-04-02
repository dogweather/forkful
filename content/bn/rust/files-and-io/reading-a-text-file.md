---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:20.494804-06:00
description: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\
  \u09CD\u099B\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09A1\u09BF\u09B8\u09CD\u0995\
  \u09C7 \u09A5\u09BE\u0995\u09BE \u098F\u0995\u099F\u09BF .txt \u09AB\u09BE\u0987\
  \u09B2 \u09A5\u09C7\u0995\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0995\u09A8\
  \u099F\u09C7\u09A8\u09CD\u099F \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F\
  \ \u0985\u09A5\u09AC\u09BE \u09AC\u09A1\u09BC\u2026"
lastmod: '2024-03-17T18:47:43.828267-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\
  \u099B\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7\
  \ \u09A5\u09BE\u0995\u09BE \u098F\u0995\u099F\u09BF .txt \u09AB\u09BE\u0987\u09B2\
  \ \u09A5\u09C7\u0995\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0995\u09A8\u099F\
  \u09C7\u09A8\u09CD\u099F \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F\
  \ \u0985\u09A5\u09AC\u09BE \u09AC\u09A1\u09BC\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কি এবং কেন?
একটি টেক্সট ফাইল পড়া মানে হচ্ছে আপনার ডিস্কে থাকা একটি .txt ফাইল থেকে টেক্সট কনটেন্ট গ্রহণ করা। প্রোগ্রামাররা কনফিগারেশন, ব্যবহারকারীর ইনপুট অথবা বড় পরিমাণে টেক্সট প্রক্রিয়া করার জন্য এটি করে থাকেন।

## কিভাবে:
রাস্টের স্ট্যান্ডার্ড লাইব্রেরি ফাইল পড়াটাকে সহজ করে দিয়েছে।

```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("example.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("ফাইলের বিষয়বস্তু:\n{}", contents);
    Ok(())
}
```
এই কোডটি "example.txt" ওপেন করে, পড়ে এবং কনটেন্ট প্রিন্ট করে।

নমুনা আউটপুট:
```
ফাইলের বিষয়বস্তু:
Hello, Rustaceans!
```

## গভীরে যাওয়া
ঐতিহাসিকভাবে, ফাইল IO জটিল হতে পারে, কিন্তু রাস্ট এটি সহজ করেছে। `read_to_string` এর পরিবর্তে বিকল্প রয়েছে, যেমন `BufRead` ব্যবহার করে প্রতি-লাইনে হ্যান্ডলিং, যা বড় ফাইলগুলিতে আরও দক্ষ। অন্তর্নিহিতভাবে, রাস্টের ফাইল পড়ার ক্ষমতা OS-স্তরের সিস্টেম কল ব্যবহার করে, দক্ষতার জন্য ডেটা বাফারিং করে।

রাস্ট 1.0 এর পর থেকে, ভাষাটি নিরাপদ সিস্টেম ইন্টার্যাকশনকে গুরুত্ব দিয়েছে - ফাইল পড়া কোনো ব্যতিক্রম নয়। `Result` টাইপ সম্ভাব্য ত্রুটিগুলি নির্বাচন করে, যা রাস্টকে মিসিং ফাইল বা অনুমতির সমস্যা প্যানিকস ছাড়াই মোকাবেলা করতে দৃঢ় করে তোলে।

## আরও দেখুন
পরীক্ষা করার জন্য অতিরিক্ত সম্পদ:
- রাস্টের ডকুমেন্টেশন ফাইল I/O সম্পর্কে: [std::fs](https://doc.rust-lang.org/std/fs/)
- বুকের ত্রুটি হ্যান্ডলিং অধ্যায়: [ত্রুটি হ্যান্ডলিং](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- রাস্ট বাই উদাহরণে ফাইল I/O: [ফাইল I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
