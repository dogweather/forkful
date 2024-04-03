---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:27.971043-06:00
description: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\
  \u09B0 \u09B8\u09AE\u09BE\u09A8 \u0995\u09BF \u09A8\u09BE \u09A6\u09C7\u0996\u09BE\
  , \u0985\u09A5\u09AC\u09BE \u098F\u0995\u099F\u09BF \u0985\u09A8\u09CD\u09AF\u099F\
  \u09BF\u09B0 \u0986\u0997\u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u098F\u09B8\u09C7\
  \u099B\u09C7 \u0995\u09BF \u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\u0987\
  \ \u0995\u09B0\u09BE\u0964 \u0995\u09CB\u09A1 \u09B2\u09BF\u0996\u09BF\u09DF\u09C7\
  \u09B0\u09BE \u0998\u099F\u09A8\u09BE\u09AC\u09B2\u09C0 \u09B8\u09BE\u099C\u09BE\
  \u09A8\u09CB, \u0987\u09A8\u09AA\u09C1\u099F \u09AF\u09BE\u099A\u09BE\u0987, \u09AE\
  \u09C7\u09DF\u09BE\u09A6\u2026"
lastmod: '2024-03-17T18:47:43.822340-06:00'
model: gpt-4-0125-preview
summary: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09B8\u09AE\u09BE\u09A8 \u0995\u09BF \u09A8\u09BE \u09A6\u09C7\u0996\u09BE, \u0985\
  \u09A5\u09AC\u09BE \u098F\u0995\u099F\u09BF \u0985\u09A8\u09CD\u09AF\u099F\u09BF\
  \u09B0 \u0986\u0997\u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u098F\u09B8\u09C7\u099B\
  \u09C7 \u0995\u09BF \u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\
  \u09B0\u09BE\u0964 \u0995\u09CB\u09A1 \u09B2\u09BF\u0996\u09BF\u09DF\u09C7\u09B0\
  \u09BE \u0998\u099F\u09A8\u09BE\u09AC\u09B2\u09C0 \u09B8\u09BE\u099C\u09BE\u09A8\
  \u09CB, \u0987\u09A8\u09AA\u09C1\u099F \u09AF\u09BE\u099A\u09BE\u0987, \u09AE\u09C7\
  \u09DF\u09BE\u09A6 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE, \u098F\u09AC\
  \u0982 \u09B8\u09AE\u09DF\u0995\u09BE\u09B2 \u0985\u09A8\u09C1\u09B8\u09B0\u09A3\
  \ \u0995\u09B0\u09BE \u0987\u09A4\u09CD\u09AF\u09BE\u09A6\u09BF\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u0964."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কি এবং কেন?
দুটি তারিখের তুলনা মানে তাদের সমান কি না দেখা, অথবা একটি অন্যটির আগে বা পরে এসেছে কি না তা যাচাই করা। কোড লিখিয়েরা ঘটনাবলী সাজানো, ইনপুট যাচাই, মেয়াদ পরিচালনা, এবং সময়কাল অনুসরণ করা ইত্যাদির জন্য এটি ব্যবহার করে থাকে।

## কিভাবে:
Rust তারিখ সহজে নিয়ন্ত্রণ করার জন্য `chrono` ব্যবহার করে। প্রথমে `cargo.toml`-এ `chrono = "0.4"` যোগ করতে হবে। তারপর আপনি এভাবে তারিখগুলি তুলনা করতে পারেন:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let date1: DateTime<Utc> = Utc::now();
    let date2: DateTime<Utc> = Utc::now(); // ভিন্ন ফলাফলের জন্য এটি পরিবর্তন করুন

    if date1 > date2 {
        println!("তারিখ১ তারিখ২ এর চেয়ে পরে");
    } else if date1 < date2 {
        println!("তারিখ১ তারিখ২ এর চেয়ে আগে");
    } else {
        println!("তারিখ১ তারিখ২ এর সমান");
    }
}
```

`date1` যদি পরে হয় তবে নমুনা আউটপুট:

```
তারিখ১ তারিখ২ এর চেয়ে পরে
```

## গভীর ডুব
অতীতে, Rust এর প্রাথমিক দিনগুলিতে (২০১০-এর দশক), তারিখের তুলনা আরও জটিল ছিল—`chrono` ক্রেট ছিল না। `chrono` এসে ধরনগুলি যেমন `DateTime` এর সাথে জিনিসগুলো সরল করে দিয়েছে। `chrono` আগে, আমরা ম্যানুয়ালি সময় নিয়ন্ত্রণ করতাম, যা ভুলের ঝুঁকিতে ছিল।

`chrono` কেন? এটি টাইম জোন এবং লিপ ইয়ার এর মতো জটিলতাগুলি অ্যাবস্ট্রাক্ট করে, তারিখের তুলনাকে নির্ভরযোগ্য করে তোলে। না হলে, আপনাকে ইউনিক্স টাইমস্ট্যাম্প নিয়ে জাগলিং করতে হত, যা ব্যবহার করা বেশ জটিল এবং কম পাঠযোগ্য।

`chrono` ছাড়াও অন্যান্য বিকল্প আছে, যেমন `time` ক্রেট, তবে `chrono` তার সরলতা এবং বৈশিষ্ট্যের জন্য ব্যাপকভাবে ব্যবহৃত হয়ে থাকে।

## দেখুন
- `chrono` ক্রেট ডকুমেন্টেশন: [docs.rs/chrono](https://docs.rs/chrono/)
- Rust এর অফিসিয়াল তারিখ এবং সময় ধারণা ডক: [doc.rust-lang.org/std/time](https://doc.rust-lang.org/std/time/index.html)
- `chrono` এবং `time` ক্রেটের তুলনা: [users.rust-lang.org](https://users.rust-lang.org/t/chrono-vs-time/45575)
