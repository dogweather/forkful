---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:40.848057-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\
  \u09A5\u09BE\u09AA\u09A8 \u09B9\u09B2\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09CB\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09A8\
  \u09CD\u09AF \u0995\u09BF\u099B\u09C1\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AC\u09A6\
  \u09B2\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.793319-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\
  \u09A5\u09BE\u09AA\u09A8 \u09B9\u09B2\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09CB\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09A8\
  \u09CD\u09AF \u0995\u09BF\u099B\u09C1\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AC\u09A6\
  \u09B2\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE \u09B8\u09AE\u09CD\
  \u09AA\u09BE\u09A6\u09A8\u09BE \u0995\u09B0\u09A4\u09C7, \u0995\u09CB\u09A1 \u09B0\
  \u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0 \u0995\u09B0\u09A4\u09C7\
  , \u0985\u09A5\u09AC\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AE\u09CD\u09AF\
  \u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8 \u0985\u099F\u09CB\u09AE\u09C7\
  \u099F \u0995\u09B0\u09A4\u09C7 \u098F\u0987 \u0995\u09BE\u099C \u0995\u09B0\u09C7\
  \ \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
```Rust
fn main() {
    let text = "Hello there!";
    let updated_text = text.replace("there", "world");
    println!("{}", updated_text); // প্রিন্ট করে "Hello world!"
}
```

নমুনা আউটপুট:
```
Hello world!
```

## গভীর ভাবনা
শুরুর দিকের টেক্সট এডিটরগুলো প্রকাশ পাওয়ার সময় থেকেই টেক্সট অনুসন্ধান এবং প্রতিস্থাপন পদ্ধতি প্রচলিত হয়ে আসছে। ইউনিক্সের সেডের মতো টুলস ব্যাচ টেক্সট প্রসেসিংকে সাধারণ অনুশীলনে পরিণত করেছে।

রাস্ট একটি কার্যকর, নিরাপদ পদ্ধতিতে এটি নেওয়া হয়। মানক লাইব্রেরির `str` টাইপের `replace` মেথডটি সোজাসাপটা এবং কম্পাইল টাইমে চেক করে।

`replace`-এর বিকল্পগুলো জটিল নকশার জন্য regex অন্তর্ভুক্ত করে, অথবা প্রতিস্থাপন যুক্তি কাস্টমাইজ করতে অক্ষরগুলো পুনরাবৃত্তি করা।

আন্তরিক ভাবে, রাস্টে `replace` একটি নতুন `String` তৈরি করে, মূল অংশটির মাধ্যমে ইটারেট করে, মেলে ধরে, এবং তারপর প্রতিস্থাপনগুলি সহ নতুন স্ট্রিং নির্মাণ করে। এটি ইউনিকোড‌কে ভালভাবে সামাল দেয়, যা তুচ্ছ নয়।

## আরও দেখুন
- `replace` সম্পর্কে রাস্টের ডকুমেন্টেশন: https://doc.rust-lang.org/std/primitive.str.html#method.replace
- আরও জটিল ব্যবহারের ক্ষেত্রের জন্য Regex crate: https://crates.io/crates/regex
- ঐতিহাসিক রেফারেন্সের জন্য সেডের ম্যানুয়াল: https://www.gnu.org/software/sed/manual/sed.html
