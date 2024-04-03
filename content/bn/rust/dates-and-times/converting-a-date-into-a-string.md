---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:29.107749-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  \u09C7\u09B0 `chrono` \u0995\u09CD\u09B0\u09C7\u099F \u09B9\u09B2 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09AA\u09B0\u09BF\u099A\
  \u09BE\u09B2\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09BE\u09AC\u09A4\
  \u09C0\u09AF\u09BC \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8\u0964 \u09A8\u09BF\u09B6\
  \u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8 \u098F\u099F\u09BF \u0986\u09AA\
  \u09A8\u09BE\u09B0 `Cargo.toml` \u098F \u0986\u099B\u09C7."
lastmod: '2024-03-17T18:47:43.821274-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7\u09B0 `chrono` \u0995\u09CD\u09B0\u09C7\
  \u099F \u09B9\u09B2 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09AF\u09BC \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AF\u09BE\u09AC\u09A4\u09C0\u09AF\u09BC \u09B8\u09AE\u09BE\u09A7\
  \u09BE\u09A8\u0964 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\
  \u09A8 \u098F\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 `Cargo.toml` \u098F \u0986\
  \u099B\u09C7."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
রাস্টের `chrono` ক্রেট হল তারিখ এবং সময় পরিচালনার জন্য যাবতীয় সমাধান। নিশ্চিত করুন এটি আপনার `Cargo.toml` এ আছে:

```toml
[dependencies]
chrono = "0.4"
```

এখন, চলুন একটি তারিখকে স্ট্রিং আকারে ফরম্যাট করা যাক।

```rust
extern crate chrono;
use chrono::{DateTime, Utc, NaiveDateTime};

fn main() {
    let date: DateTime<Utc> = Utc::now(); // বর্তমান UTC তারিখ ও সময় পান।
    let formatted_date = date.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("{}", formatted_date); // প্রিন্ট করে: 2023-03-15 14:30:45
}
```

## গভীর ডাইভ
`chrono` এর আগে, রাস্টের স্ট্যান্ডার্ড লাইব্রেরিতে কিছু তারিখ ও সময় ফাংশন ছিল, কিন্তু তারা মৌলিক ছিল। `chrono` ওই মৌলিক ভিত্তির উপর নির্মিত হয়েছে যাতে সম্পূর্ণ কার্যকারিতা প্রদান করা যায়। একটি বিকল্প হতে পারে রাস্টের নতুন `time` ক্রেট, যা একটি নিরাপদ এবং অধিক এরগনোমিক API এর লক্ষ্য নিয়েছে।

যখন আপনি একটি তারিখকে স্ট্রিং আকারে রূপান্তর করেন, আপনি সিরিয়ালাইজ করেন - ডেটা এমন একটি ফর্ম্যাটে পরিণত করেন যা শেয়ার করা বা সংরক্ষণ করা সম্ভব। আপনি যে ফরম্যাট চয়ন করেন (আমাদের ক্ষেত্রে `%Y-%m-%d %H:%M:%S`) তা আপনার উপর নির্ভর করে, এবং `chrono` এরকম অনেক প্যাটার্ন সমর্থন করে।

অভ্যন্তরীণভাবে, তারিখগুলি প্রায়শই টাইমস্ট্যাম্প হিসাবে সংরক্ষিত হয় - যেমন ইউনিক্স এপোক (জানুয়ারী ১, ১৯৭০) থেকে শুরু করে সেকেন্ড। যখন আপনি একটি তারিখ ফরম্যাট করেন, আপনি এই গণনা থেকে মানব-পাঠ্য রূপ কম্পিউট করেন, সময় অঞ্চল এবং অধিবর্ষ সেকেন্ডের বিবেচনা করে।

## অতিরিক্ত দেখুন
- `chrono` ক্রেট ডকুমেন্টেশন: https://docs.rs/chrono/
- রাস্টের `time` ক্রেট ডকুমেন্টেশন: https://docs.rs/time/
- তারিখ ফরম্যাটিং সিনট্যাক্স: http://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
