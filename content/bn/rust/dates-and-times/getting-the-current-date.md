---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:03.961571-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  \u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09B0\u09CD\
  \u09A4\u09AE\u09BE\u09A8 \u09B8\u09AE\u09AF\u09BC \u09AA\u09CD\u09B0\u09BE\u09AA\
  \u09CD\u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09C0\u09AE\u09BF\u09A4\
  \ \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\
  \u09A6\u09BF\u0993 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u0995\u09CD\u09AF\u09BE\
  \u09B2\u09C7\u09A8\u09CD\u09A1\u09BE\u09B0 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \u2026"
lastmod: '2024-03-17T18:47:43.820223-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09B8\u09AE\u09AF\
  \u09BC \u09AA\u09CD\u09B0\u09BE\u09AA\u09CD\u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09B8\u09C0\u09AE\u09BF\u09A4 \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09A6\
  \u09CD\u09B0\u09C1\u09A4 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09A6\u09BF\u0993 \u09B8\u09B0\u09BE\u09B8\
  \u09B0\u09BF \u0995\u09CD\u09AF\u09BE\u09B2\u09C7\u09A8\u09CD\u09A1\u09BE\u09B0\
  \ \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09A8\u09AF\u09BC\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:


### রাস্টের স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে
রাস্টের স্ট্যান্ডার্ড লাইব্রেরি বর্তমান সময় প্রাপ্তির জন্য সীমিত কিন্তু দ্রুত উপায় প্রদান করে, যদিও সরাসরি ক্যালেন্ডার ফরম্যাটে বর্তমান তারিখ নয়। এখানে আপনি কিভাবে এটি করতে পারেন:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("বর্তমান সময়: ইউনিক্স এপক থেকে {} সেকেন্ড।", n.as_secs()),
        Err(_) => panic!("সিস্টেমটাইম ইউনিক্স এপকের আগে!"),
    }
}
```

আউটপুট:
```
বর্তমান সময়: ইউনিক্স এপক থেকে 1615390665 সেকেন্ড।
```

### ক্রোনো লাইব্রেরি ব্যবহার করে
আরো সম্পূর্ণ তারিখ এবং সময় ফাংশনালিটির জন্য, যেমন বর্তমান তারিখ প্রাপ্তির জন্য, আপনাকে `chrono` লাইব্রেরি ব্যবহার করা উচিত। প্রথমে, `Cargo.toml`-এ `chrono` যোগ করুন:

```toml
[dependencies]
chrono = "0.4"
```

তারপর, বর্তমান তারিখ প্রাপ্তির জন্য `chrono` ব্যবহার করুন:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("বর্তমান তারিখ: {}-{}-{}", now.year(), now.month(), now.day());
}
```

আউটপুট:
```
বর্তমান তারিখ: 2023-4-20
```

`chrono` লাইব্রেরি তারিখ এবং সময় নিয়ে কাজ করা সোজা করে দেয়, শুধুমাত্র বর্তমান তারিখ প্রাপ্তির ফাংশনালিটি অতিক্রম করে এটি তারিখ এবং সময়ে পার্সিং, ফর্ম্যাটিং এবং অঙ্কন কাজে একবিস্তারিত সুযোগ প্রদান করে।
