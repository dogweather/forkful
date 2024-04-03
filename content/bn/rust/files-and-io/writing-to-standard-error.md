---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:31.213814-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  \ stderr \u09A4\u09C7 \u09B2\u09BF\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `eprintln!`\
  \ \u09AE\u09CD\u09AF\u09BE\u0995\u09CD\u09B0\u09CB\u0995\u09C7 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09C7\
  \u09AE\u09A8\u099F\u09BE `println!` \u0995\u09C7 stdout \u098F\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\
  \u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.827224-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F stderr \u09A4\u09C7 \u09B2\u09BF\u0996\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF `eprintln!` \u09AE\u09CD\u09AF\u09BE\u0995\u09CD\
  \u09B0\u09CB\u0995\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8\u099F\u09BE `println!`\
  \ \u0995\u09C7 stdout \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
রাস্ট stderr তে লিখার জন্য `eprintln!` ম্যাক্রোকে ব্যবহার করে সরল উপায় প্রদান করে, যেমনটা `println!` কে stdout এর জন্য ব্যবহার করা হয়। এখানে একটি মৌলিক উদাহরণ রয়েছে:

```rust
fn main() {
    eprintln!("This is an error message!");
}
```

নমুনা আউটপুট (স্ট্যান্ডার্ড এররে):
```
This is an error message!
```

ত্রুটি বার্তাগুলির উপর আরও নিয়ন্ত্রণ পেতে, যেমন যদি আপনি টেক্সট ফরম্যাট করতে বা I/O ফলাফলগুলি হ্যান্ডল করতে চান, তবে `std::io` মডিউল থেকে `stderr` ফাংশন ব্যবহার করুন। এই পদ্ধতি গ্লোবাল stderr স্ট্রিমের একটি হ্যান্ডেল প্রদান করে, যা আপনি `Write` ট্রেইট থেকে `write_all` বা `writeln` মেথড ব্যবহার করে লিখতে পারেন:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Formatted error message: {}", 404).expect("Failed to write to stderr");
}
```

নমুনা আউটপুট (স্ট্যান্ডার্ড এররে):
```
Formatted error message: 404
```

যদি আপনি পরিবেশ বা অ্যাপ্লিকেশনে কাজ করেন যেখানে লগিং বা ত্রুটি হ্যান্ডলিংয়ের জন্য লাইব্রেরিগুলিকে নির্ভর করেন, `log` এবং `env_logger` এর মতো লাইব্রেরিগুলি জনপ্রিয়। যদিও তারা লগিং উদ্দেশ্যে ব্যবহার করা হয়, তারা কনফিগার যোগ্য এবং ত্রুটি লগ স্তরগুলিকে stderr এ নির্দেশ করতে পারে। নীচে `log` এবং `env_logger` ব্যবহার করে একটি সহজ উদাহরণ দেওয়া হল:

প্রথমে, আপনার `Cargo.toml` এ নির্ভরতাগুলি যোগ করুন:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

তারপর, আপনার অ্যাপ্লিকেশনে লগিং সেটআপ ও ব্যবহার করুন:
```rust
fn main() {
    env_logger::init();
    log::error!("This is an error message logged to stderr");
}
```

এই প্রোগ্রামটি রান করার (যেমন, উপযুক্ত পরিবেশ ভেরিয়েবলে `RUST_LOG=error` সেট আপ করার পরে) পরে ত্রুটি বার্তাটি stderr এ আউটপুট হবে, লগিং ইনফ্রাস্ট্রাকচার ব্যবহার করে।

```plaintext
ERROR: This is an error message logged to stderr
```
