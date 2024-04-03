---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:25:29.832418-06:00
description: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7, \u09AC\u09BE \u09AF\u09BE \u09B0\u09BE\u09B8\u09CD\
  \u099F\u09C7\u09B6\u09BF\u09AF\u09BC\u09BE\u09A8\u09B0\u09BE \"\u09B9\u09CD\u09AF\
  \u09BE\u09B6 \u09AE\u09CD\u09AF\u09BE\u09AA\" \u09AC\u09B2\u09C7 \u09A5\u09BE\u0995\
  \u09C7\u09A8, \u09B9\u09B2\u09CB \u09B8\u0982\u0997\u09CD\u09B0\u09B9\u09B8\u09CD\
  \u09A5\u09B2 \u09AF\u09BE \u09A1\u09C7\u099F\u09BE \u0995\u09C0-\u09AE\u09BE\u09A8\
  \ \u099C\u09CB\u09A1\u09BC\u09BE\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09B8\u0982\
  \u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0997\u09C1\u09B2\u09BF\
  \ \u09A6\u09CD\u09B0\u09C1\u09A4 \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.801841-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7, \u09AC\u09BE \u09AF\u09BE \u09B0\u09BE\u09B8\u09CD\
  \u099F\u09C7\u09B6\u09BF\u09AF\u09BC\u09BE\u09A8\u09B0\u09BE \"\u09B9\u09CD\u09AF\
  \u09BE\u09B6 \u09AE\u09CD\u09AF\u09BE\u09AA\" \u09AC\u09B2\u09C7 \u09A5\u09BE\u0995\
  \u09C7\u09A8, \u09B9\u09B2\u09CB \u09B8\u0982\u0997\u09CD\u09B0\u09B9\u09B8\u09CD\
  \u09A5\u09B2 \u09AF\u09BE \u09A1\u09C7\u099F\u09BE \u0995\u09C0-\u09AE\u09BE\u09A8\
  \ \u099C\u09CB\u09A1\u09BC\u09BE\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09B8\u0982\
  \u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0997\u09C1\u09B2\u09BF\
  \ \u09A6\u09CD\u09B0\u09C1\u09A4 \u09A1\u09C7\u099F\u09BE \u09B2\u09C1\u0995\u0986\
  \u09AA \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7\u09A8, \u09AF\u09BE \u0985\u09A8\u09A8\u09CD\u09AF \u0995\
  \u09C0 \u098F\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u09A4\u09C7 \u09A1\u09BE\
  \u099F\u09BE \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u09A6\u0995\u09CD\
  \u09B7 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964."
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
weight: 15
---

## কিভাবে:
এ হ্যাশম্যাপগুলি নিয়ে কাজ করা যায়:

```Rust
use std::collections::HashMap;

fn main() {
    // একটি নতুন HashMap তৈরি করা
    let mut scores = HashMap::new();

    // মান যোগ করা
    scores.insert(String::from("নীল"), 10);
    scores.insert(String::from("হলুদ"), 50);

    // মান অ্যাক্সেস করা
    let team_name = String::from("নীল");
    if let Some(score) = scores.get(&team_name) {
        println!("নীল দলের স্কোর: {}", score); // আউটপুট: নীল দলের স্কোর: 10
    }

    // একটি মান আপডেট করা
    scores.entry(String::from("নীল")).and_modify(|e| *e += 5);

    // কী-মান জোড়া নিয়ে অভিগমন
    for (key, value) in &scores {
        println!("{}: {}", key, value); // আউটপুট: নীল: 15, হলুদ: 50
    }
}
```

## গভীর ডুব
রাস্টের `HashMap` কীগুলিকে মান অনুসন্ধান করতে একটি হ্যাশিং ফাংশন ব্যবহার করে, যা দ্রুত ডেটা পুনরুদ্ধারকে সক্ষম করে। তবে, এই দক্ষতার একটা খরচ হলো: হ্যাশ ম্যাপ তার উপাদানগুলির অর্ডার বজায় রাখে না। এটি অন্যান্য এসোসিয়েটিভ অ্যারে বাস্তবায়নের সাথে প্রতিকূল, যেমন পাইথনের (`dict`) বা রুবির, যা সাম্প্রতিক সংস্করণে ইনসারশন অর্ডার বজায় রাখার একটি বৈশিষ্ট্য হিসেবে রাখে। কী-মান জোড়াগুলির অর্ডার গুরুত্বপূর্ণ যেখানে, রাস্ট ডেভেলপাররা `std::collections` মডিউল থেকে `BTreeMap` ব্যবহার করতে পারে, যা অর্ডার বজায় রাখে কিন্তু `HashMap` এর তুলনায় সন্নিবেশ এবং পুনরুদ্ধার ধীর হতে পারে। শেষ পর্যন্ত, `HashMap` এবং `BTreeMap` এর মধ্যে বাছাই অর্ডারিং এবং পারফরমেন্সের চারপাশে নির্দিষ্ট প্রয়োজনীয়তা উপর নির্ভর করে।
