---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:59.602282-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\
  \u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0\
  \ \u09AA\u09B0 \u09AC\u09BE \u0986\u0997\u09C7 \u0995\u09CD\u09AF\u09BE\u09B2\u09C7\
  \u09A8\u09CD\u09A1\u09BE\u09B0 \u0995\u09BF \u09A6\u09C7\u0996\u09BE\u09AC\u09C7\
  \ \u09A4\u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09AE\u09AF\u09BC\
  -\u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4 \u09AF\u09C7\u0995\u09CB\
  \u09A8\u09CB \u0995\u09BF\u099B\u09C1\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:43.823454-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\
  \u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09AA\
  \u09B0 \u09AC\u09BE \u0986\u0997\u09C7 \u0995\u09CD\u09AF\u09BE\u09B2\u09C7\u09A8\
  \u09CD\u09A1\u09BE\u09B0 \u0995\u09BF \u09A6\u09C7\u0996\u09BE\u09AC\u09C7 \u09A4\
  \u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09AE\u09AF\u09BC-\u09B8\
  \u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4 \u09AF\u09C7\u0995\u09CB\u09A8\u09CB\
  \ \u0995\u09BF\u099B\u09C1\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8, \u09AF\u09C7\u09AE\u09A8 \u09B0\u09BF\
  \u09AE\u09BE\u0987\u09A8\u09CD\u09A1\u09BE\u09B0 \u09B8\u09C7\u099F \u0995\u09B0\
  \u09BE, \u09AE\u09C7\u09AF\u09BC\u09BE\u09A6 \u0989\u09A4\u09CD\u09A4\u09C0\u09B0\
  \u09CD\u09A3\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09A8\u09BF\u09B0\u09CD\
  \u09A7\u09BE\u09B0\u09A3 \u09AC\u09BE \u0987\u09AD\u09C7\u09A8\u09CD\u099F \u09B8\
  \u09BF\u09A1\u09BF\u0989\u09B2 \u0995\u09B0\u09BE\u0964."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
রাস্টে আপনার তারিখ ও সময়ের প্রয়োজন পূরণের জন্য `chrono` ক্রেট রয়েছে। এখানে তারিখে যোগ বা বিয়োগ করার উপায় দেওয়া হল:

```rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("বর্তমান UTC সময়: {}", now);

    let two_weeks = Duration::weeks(2);
    let future_date = now + two_weeks;
    println!("দুই সপ্তাহ পরের UTC: {}", future_date);

    let thirty_days_ago = Duration::days(-30);
    let past_date = now + thirty_days_ago;
    println!("UTC ৩০ দিন আগে: {}", past_date);
}
```

নমুনা আউটপুট:

```
বর্তমান UTC সময়: 2023-04-01T12:00:00Z
দুই সপ্তাহ পরের UTC: 2023-04-15T12:00:00Z
UTC ৩০ দিন আগে: 2023-03-02T12:00:00Z
```

## গভীর ডুব
প্রথাগতভাবে, তারিখ এবং সময় সম্পর্কে ম্যানিপুলেশন কষ্টকর ছিল। বিভিন্ন সিস্টেম এবং প্রোগ্রামিং ভাষায় এটি নানাভাবে হ্যান্ডল করা হয়। রাস্টের স্ট্যান্ডার্ড লাইব্রেরি মৌলিক কার্যকারিতা সরবরাহ করে, তবে `chrono` ক্রেট পছন্দের অপশন হয়ে উঠেছে।

বিকল্প? অবশ্যই, আপনি সমস্ত কিছুকে টাইমস্ট্যাম্পে রূপান্তর করে, সংখ্যাগুলি ম্যানিপুলেট করে এবং ফিরে রূপান্তর করে ম্যানুয়ালি তারিখ গণনা করতে পারেন। অথবা, আপনি অন্য ভাষার সময়-নির্দিষ্ট লাইব্রেরি ব্যবহার করতে পারেন—পাইথনের `datetime`, জাভাস্ক্রিপ্টের `Date` ইত্যাদি।

রাস্টের `chrono` ক্রেট আপনাকে টাইম-জোন সচেতন ধরণের যেমন `DateTime`, এবং উপরে দেখানো মতো সময়কাল দেয়। এটি অধিবর্ষ এবং ডেলাইট সেভিংসের মতো জটিল বিষয়গুলি সামলায় যাতে আপনাকে এগুলি নিয়ে চিন্তা করতে না হয়। এটি তারিখ পারসিং এবং বিন্যাস করাও সম্পন্ন করে, যা এটিকে একটি সম্পূর্ণ সমাধান হিসেবে গড়ে তোলে।

## দেখুন এছাড়া
- `chrono` ক্রেট: https://crates.io/crates/chrono
- রাস্টের সময় ডকুমেন্টেশন: https://doc.rust-lang.org/std/time/index.html
- "The Rust Programming Language" বইয়ের রাস্ট ডেট এবং সময় অধ্যায়: https://doc.rust-lang.org/book/ch10-02-traits.html (DateTime-সম্পর্কিত অংশগুলি খুঁজুন)
