---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:11.878930-06:00
description: "\u09B0\u09BE\u09B8\u09CD\u099F \u098F \u0995\u09AE\u09BE\u09A8\u09CD\
  \u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\
  \u09CD\u099F \u09AA\u09A1\u09BC\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u0995\u09C7 \u099A\u09BE\u09B2\u09C1\u09B0 \u09B8\u09AE\u09AF\u09BC\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\
  \u09A8\u09AA\u09C1\u099F \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u09B8\u09C1\
  \u09AF\u09CB\u0997 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\u09BF \u098F\u0995\
  \u099F\u09BF GUI \u099B\u09BE\u09A1\u09BC\u09BE \u0995\u09BE\u09B8\u09CD\u099F\u09AE\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09AA\u09CD\u09B0\u09A7\u09BE\u09A8\u0964"
lastmod: '2024-03-17T18:47:43.826155-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F \u098F \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\
  \u099F \u09AA\u09A1\u09BC\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u0995\u09C7 \u099A\u09BE\u09B2\u09C1\u09B0 \u09B8\u09AE\u09AF\u09BC \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\
  \u09C1\u099F \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u09B8\u09C1\u09AF\u09CB\
  \u0997 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF\
  \ GUI \u099B\u09BE\u09A1\u09BC\u09BE \u0995\u09BE\u09B8\u09CD\u099F\u09AE \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\
  \u09CD\u09B0\u09A7\u09BE\u09A8\u0964"
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?

রাস্ট এ কমান্ড লাইন আর্গুমেন্ট পড়া প্রোগ্রামকে চালুর সময় ব্যবহারকারীর ইনপুট নেওয়ার সুযোগ দেয়। এটি একটি GUI ছাড়া কাস্টম ব্যবহারের জন্য প্রধান।

## কিভাবে:

এখানে আর্গুমেন্ট নেওয়ার সবচেয়ে সহজ উপায় দেওয়া হল:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

এটি `cargo run arg1 arg2` দ্বারা চালান। আপনি দেখবেন:

```
["path/to/executable", "arg1", "arg2"]
```

ইটারেটরস দিয়ে আরও গোছানো একটি বিকল্প:

```Rust
use std::env;

fn main() {
    for arg in env::args().skip(1) {
        println!("{}", arg);
    }
}
```

এখন `cargo run cool stuff` ট্রাই করুন:

```
cool
stuff
```

## গভীর ডুব

ঐতিহাসিকভাবে, কমান্ড লাইন আর্গুমেন্টগুলি ঐ দিনগুলির প্রতিধ্বনি, যখন GUI সর্বব্যাপী ছিল না। এখন, তারা স্ক্রিপ্ট, সার্ভার, অথবা টুলস এর জন্য দারুণ।

রাস্টের `std::env::args` একটি ইটারেটর ব্যবহার করে, যা মেমোরি দক্ষ এবং অলস। এটি ইউনিকোড হ্যান্ডেল করে। রয়েছে `args_os` ওএস স্ট্রিংসের জন্য।

জটিল পার্সিং এর জন্য, `clap` অথবা `structopt` মত ক্রেটগুলি দরকারী হয়। এগুলি ফ্ল্যাগস, অপশনস এবং সাবকম্যান্ডস পার্স করে।

## আরও দেখুন

- [রাস্ট `std::env` মডিউল](https://doc.rust-lang.org/std/env/)
- [`clap` ক্রেট ডকুমেন্টেশন](https://docs.rs/clap/)
- [কমান্ড লাইন আর্গুমেন্টগুলি উপর রাস্ট বুক](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
