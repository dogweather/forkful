---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:31.213814-06:00
description: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\
  \u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u09A4\u09C7\
  \ \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE\
  \ \u098F\u09AC\u0982 \u09A1\u09BE\u09AF\u09BC\u09BE\u0997\u09A8\u09B8\u09CD\u099F\
  \u09BF\u0995\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ (stdout) \u09A5\u09C7\u0995\u09C7 \u09AA\u09C3\u09A5\u0995\u09AD\u09BE\u09AC\u09C7\
  \ \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\
  \u2026"
lastmod: '2024-03-17T18:47:43.827224-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\
  \u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u09A4\u09C7\
  \ \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE\
  \ \u098F\u09AC\u0982 \u09A1\u09BE\u09AF\u09BC\u09BE\u0997\u09A8\u09B8\u09CD\u099F\
  \u09BF\u0995\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ (stdout) \u09A5\u09C7\u0995\u09C7 \u09AA\u09C3\u09A5\u0995\u09AD\u09BE\u09AC\u09C7\
  \ \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\
  \u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
---

{{< edit_this_page >}}

## কি এবং কেন?
রাস্টে স্ট্যান্ডার্ড এরর (stderr) তে লেখা মানে হল প্রোগ্রামের বার্তা এবং ডায়াগনস্টিকসগুলিকে স্ট্যান্ডার্ড আউটপুট (stdout) থেকে পৃথকভাবে কনসোলে নির্দেশ করা। প্রোগ্রামাররা এটি সাধারণ প্রোগ্রাম আউটপুট থেকে ত্রুটি বার্তাগুলি পৃথক করতে করে, এতে ত্রুটিগুলি সঠিকভাবে নিয়ন্ত্রণ করা সহজ হয় অথবা এগুলিকে লগ বা ফাইলে পুনঃনির্দেশ করা যায় রান টাইমের সময়ে।

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
