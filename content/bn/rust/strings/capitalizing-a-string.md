---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:41.472640-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Rust-\u098F \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09CD\u09AF\u09BE\u09AA\
  \u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u09A6\u09C1\u099F\u09BF \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\
  \u09BF\u0995 \u0989\u09AA\u09BE\u09AF\u09BC \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\
  : \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u0995\u09BE\u09B0\
  \u09CD\u09AF\u0995\u09B2\u09BE\u09AA \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE \u0986\u09B0\u0993 \u099C\u099F\u09BF\
  \u09B2 \u09AC\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\u2026"
lastmod: '2024-03-17T18:47:43.791258-06:00'
model: gpt-4-0125-preview
summary: "Rust-\u098F \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C\
  \ \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 \u09A6\u09C1\u099F\u09BF\
  \ \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995 \u0989\u09AA\u09BE\u09AF\u09BC\
  \ \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
Rust-এ একটি স্ট্রিং ক্যাপিটালাইজ করতে, আপনার দুটি প্রাথমিক উপায় রয়েছে: স্ট্যান্ডার্ড লাইব্রেরির কার্যকলাপ ব্যবহার করা অথবা আরও জটিল বা নির্দিষ্ট প্রয়োজনের জন্য থার্ড-পার্টি ক্রেটস ব্যবহার করা। এখানে আপনি উভয় উপায়ে কিভাবে করতে পারেন তা দেখানো হয়েছে।

### Rust-এর স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে
Rust-এর স্ট্যান্ডার্ড লাইব্রেরি সরাসরি স্ট্রিং ক্যাপিটালাইজ করার কোন পদ্ধতি প্রদান করে না, কিন্তু আপনি স্ট্রিংটির অক্ষরগুলি পরিবর্তন করে এটা অর্জন করতে পারেন।

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // আউটপুট: Hello
}
```

### `heck` ক্রেট ব্যবহার করে
একটি সোজা পথ বিশেষ করে বৃহত্তর টেক্সট প্রক্রিয়াজাতকরণ প্রসঙ্গে কাজ করার সময়, আপনি থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করতে পছন্দ করতে পারেন যেমন `heck`। `heck` ক্রেট বিভিন্ন কেস রূপান্তর কার্যকলাপ সরবরাহ করে, যার মধ্যে স্ট্রিং ক্যাপিটালাইজ করার একটি সহজ উপায় রয়েছে।

প্রথমে, আপনার `Cargo.toml` এ `heck` যোগ করুন:

```toml
[dependencies]
heck = "0.4.0"
```

তারপর, আপনার স্ট্রিং ক্যাপিটালাইজ করতে এটি ব্যবহার করুন:

```rust
extern crate heck; // Rust 2018 সংস্করণ বা পরে আবশ্যক নেই
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // আউটপুট: Hello World
}
```

মনে রাখবেন: `heck` দ্বারা প্রদান করা `to_title_case` পদ্ধতি স্ট্রিংটির প্রতিটি শব্দের প্রথম অক্ষর ক্যাপিটালাইজ করে, যা আপনি যে শুধু স্ট্রিংটির প্রথম অক্ষর ক্যাপিটালাইজ করতে চান তার চেয়ে বেশি হতে পারে। আপনার নির্দিষ্ট প্রয়োজন অনুযায়ী আপনার ব্যবহার সামঞ্জস্য করুন।
