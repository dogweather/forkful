---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:08.897475-06:00
description: "TOML \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09A8\u09C1\u09B7\u09C7\u09B0\
  \ \u09AA\u09A1\u09BC\u09BE\u09B0 \u0989\u09AA\u09AF\u09CB\u0997\u09C0 \u09A1\u09C7\
  \u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\
  \u09B6\u09A8 \u09AD\u09BE\u09B7\u09BE, \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE TOML \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u09B0 \u09B8\u09BE\u09A7\u09BE\u09B0\
  \u09A3\u09A4\u09BE \u098F\u09AC\u0982 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F\u09A4\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF,\u2026"
lastmod: '2024-03-17T18:47:43.834325-06:00'
model: gpt-4-0125-preview
summary: "TOML \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09A8\u09C1\u09B7\u09C7\u09B0\
  \ \u09AA\u09A1\u09BC\u09BE\u09B0 \u0989\u09AA\u09AF\u09CB\u0997\u09C0 \u09A1\u09C7\
  \u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\
  \u09B6\u09A8 \u09AD\u09BE\u09B7\u09BE, \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE TOML \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u09B0 \u09B8\u09BE\u09A7\u09BE\u09B0\
  \u09A3\u09A4\u09BE \u098F\u09AC\u0982 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F\u09A4\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AF\u09BE Rust \u098F \u09B9\u09CD\u09AF\
  \u09BE\u09B6 \u09AE\u09CD\u09AF\u09BE\u09AA\u09C7 \u09B8\u09B9\u099C\u09C7 \u0985\
  \u09A8\u09C1\u09AC\u09BE\u09A6 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC\u0964\
  ."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কি এবং কেন?
TOML একটি মানুষের পড়ার উপযোগী ডেটা সিরিয়ালাইজেশন ভাষা, প্রায়ই কনফিগের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা TOML ব্যবহার করে এর সাধারণতা এবং স্পষ্টতার জন্য, যা Rust এ হ্যাশ ম্যাপে সহজে অনুবাদ করা যায়।

## কিভাবে:
```Rust
// 1. আপনার Cargo.toml এ 'toml' crate যুক্ত করুন
// [dependencies]
// toml = "0.5"

// 2. Rust এ একটি struct এ TOML কে Deserialize করুন
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("সার্ভার চলছে {}:{}", host, port);
    // আউটপুট: সার্ভার চলছে "localhost":8080
}
```

## গভীর ডাইভ
TOML, যার পূর্ণ নাম Tom's Obvious, Minimal Language, টম প্রেস্টন-ওয়ার্নার দ্বারা ২০১৩ সালে সৃষ্টি করা হয়েছিল। এর লক্ষ্য হল কনফিগ ফাইলের জন্য JSON বা YAML এর চেয়ে আরও পঠনীয় হওয়া। TOML এর নকশা অস্পষ্ট সিনট্যাক্স, ন্যূনতমতার এবং ডাটা টাইপে সহজে মানচিত্রণে মনোনিবেশ করা হয়েছে।

TOML এর বিকল্পগুলি হল JSON, YAML, এবং XML, কিন্তু TOML মানব পাঠযোগ্যতা এবং অ-প্রোগ্রামারের দ্বারা ফাইল সম্পাদনা যে পরিস্থিতিগুলিতে বিশেষ গুরুত্বপূর্ণ, সেইসব পরিস্থিতিতে জয়ী হয়। Rust এ TOML এর সাথে কাজ করার সময়, serde সিরিয়ালাইজেশন এবং ডিসিরিয়ালাইজেশনের জন্য একটি শক্ত ভিত্তি প্রদান করে, TOML কে Rust এর structs এ নিয়ে আসতে ট্রেইটস ব্যবহার করে।

TOML এর সাথে কাজ করার একটি চ্যালেঞ্জ হল এর টাইপ এবং গঠনের উপর কঠোরতা। প্রোগ্রামারকে TOML ডেটার স্কিমাকে সঠিকভাবে ব্যবহার করতে Rust এর একটি ভাল গঠিত টাইপ সিস্টেম নির্ধারণ করতে হবে।

## আরও দেখুন
- [TOML ডকুমেন্টেশন](https://toml.io/en/)
- [serde_toml Crate](https://docs.rs/serde_toml/)
- [Rust প্রোগ্রামিং ভাষার বই](https://doc.rust-lang.org/stable/book/)
- [TOML GitHub রেপো](https://github.com/toml-lang/toml)
