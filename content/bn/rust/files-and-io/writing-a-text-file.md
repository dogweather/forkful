---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:40.385715-06:00
description: "\u09B0\u09BE\u09B8\u09CD\u099F \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC\
  \ \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\
  \u09B2 \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09AB\u09BE\u0987\u09B2\
  \ \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09AB\
  \u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE, \u09A4\u09BE\u09A4\
  \u09C7 \u09B2\u09C7\u0996\u09BE \u098F\u09AC\u0982 \u09B8\u09AE\u09CD\u09AD\u09AC\
  \u09A4 \u09A1\u09C7\u099F\u09BE \u09AF\u09CB\u0997 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\
  \u099F\u09BE \u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0987\u2026"
lastmod: '2024-03-17T18:47:43.829268-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u098F\
  \u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\
  \ \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B8\
  \u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09AB\u09BE\
  \u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE, \u09A4\u09BE\u09A4\u09C7\
  \ \u09B2\u09C7\u0996\u09BE \u098F\u09AC\u0982 \u09B8\u09AE\u09CD\u09AD\u09AC\u09A4\
  \ \u09A1\u09C7\u099F\u09BE \u09AF\u09CB\u0997 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\
  \u09BE \u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u0987\u2026"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কি এবং কেন?
রাস্ট ভাষায় একটি টেক্সট ফাইল লেখা মানে ফাইল সিস্টেমে একটি ফাইল তৈরি করা, তাতে লেখা এবং সম্ভবত ডেটা যোগ করা। প্রোগ্রামাররা ডেটা স্থায়ী করার জন্য এই অপারেশন সম্পাদন করেন, যেমন অ্যাপ্লিকেশন লগ, কনফিগারেশন, অথবা ব্যবহারকারী দ্বারা তৈরি কনটেন্ট, নিশ্চিত করে যে প্রোগ্রাম নির্বাহের সীমার বাইরেও ডেটা টিকে থাকে।

## কিভাবে:
রাস্টের স্ট্যান্ডার্ড লাইব্রেরি ফাইল ম্যানিপুলেশনের জন্য বলিষ্ঠ টুলস প্রদান করে, যা মূলত `std::fs` এবং `std::io` মডিউলের মধ্যে আবদ্ধ। এখানে একটি টেক্সট ফাইল তৈরি এবং লেখার একটি প্রাথমিক উদাহরণ:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

এই কোড চালানোর পর, আপনি দেখবেন "hello.txt" নামে একটি ফাইল রয়েছে যার বিষয়বস্তু "Hello, world!"।

আরও জটিল পরিস্থিতিতে, যেমন একটি ফাইলে টেক্সট যোগ করা অথবা বড় ডেটা দক্ষতার সাথে হ্যান্ডল করা, রাস্ট অতিরিক্ত ফাংশনালিটি অফার করে। এখানে কিভাবে একটি বিদ্যমান ফাইলে টেক্সট যোগ করবেন:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Adding more text.")?;
    Ok(())
}
```

এই কোড চালানোর ফলে “hello.txt” এর শেষে " Adding more text." যোগ হবে।

কিছু ক্ষেত্রে, তৃতীয়-পক্ষের লাইব্রেরিগুলি ফাইল অপারেশনগুলিকে সহজ করতে পারে। `serde` ক্রেট, যখন `serde_json` এর সাথে সমন্বিত করা হয়, JSON ফরম্যাটে ডেটা স্ট্রাকচারগুলি সিরিয়ালাইজ এবং ডিসিরিয়ালাইজ করার জন্য অনুমতি দেয়, ফাইল লেখার জন্য একটি উচ্চ-স্তরের পদ্ধতি অফার করে:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

উপরের কোড চালানোর পরে, `user.json` এ `User` স্ট্রাক্টের একটি JSON প্রতিনিধিত্ব থাকবে। নোট করুন, `serde` এবং `serde_json` ব্যবহার করতে আপনাকে এই ক্রেটগুলি আপনার `Cargo.toml` এ যোগ করতে হবে।

রাস্টে টেক্সট ফাইল লেখা, স্ট্যান্ডার্ড লাইব্রেরির মাধ্যমে অথবা বাইরের ক্রেটের সাহায্যে, আপনার অ্যাপ্লিকেশনের ডেটা স্থায়িত্ব পরিচালনার জন্য একটি সরল তবে শক্তিশালী উপায়।
