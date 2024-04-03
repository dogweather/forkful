---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:18.712729-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Rust\u09C7\u09B0 `regex` \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\
  \u09A4 \u09AA\u09CD\u09B0\u0995\u09BE\u09B6\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\
  \u099F\u09BF \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09B8\u09CD\u09A5\u09BE\
  \u09A8\u0964 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09AA\u09CD\u09B0\u09A5\
  \u09AE\u09C7 \u098F\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 `Cargo.toml` \u098F\
  \ \u09AF\u09CB\u0997 \u0995\u09B0\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.798480-06:00'
model: gpt-4-0125-preview
summary: "Rust\u09C7\u09B0 `regex` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u09AA\u09CD\u09B0\u0995\u09BE\
  \u09B6\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AF\u09BE\u0993\u09AF\
  \u09BC\u09BE\u09B0 \u09B8\u09CD\u09A5\u09BE\u09A8\u0964 \u098F\u099F\u09BF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\
  \u09BE\u0995\u09C7 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7 \u098F\u099F\u09BF \u0986\
  \u09AA\u09A8\u09BE\u09B0 `Cargo.toml` \u098F \u09AF\u09CB\u0997 \u0995\u09B0\u09A4\
  \u09C7 \u09B9\u09AC\u09C7."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
Rustের `regex` লাইব্রেরি নিয়মিত প্রকাশের সাথে কাজ করার জন্য একটি যাওয়ার স্থান। এটি ব্যবহার করতে, আপনাকে প্রথমে এটি আপনার `Cargo.toml` এ যোগ করতে হবে:

```toml
[dependencies]
regex = "1"
```

এরপরে, আপনি Rust কোডে রেজেক্স ফাংশনালিটিজ বাস্তবায়ন শুরু করতে পারেন। এখানে কিছু সাধারণ অপারেশনগুলি কীভাবে সম্পাদন করবেন তার একটি গাইড দেওয়া হল:

### একটি স্ট্রিংয়ে প্যাটার্ন খোঁজা
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("টেক্সটটি কি তারিখের প্যাটার্নের সাথে মিলে? {}", re.is_match(date));
    // আউটপুট: টেক্সটটি কি তারিখের প্যাটার্নের সাথে মিলে? true
}
```

### খোঁজ এবং মিলের ডাটা প্রাপ্তি
```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("ভাষা: {}, বছর: {}", &cap[1], &cap[2]);
    }
    // আউটপুট:
    // ভাষা: Rust, বছর: 2023
    // ভাষা: C++, বছর: 2022
    // ভাষা: Python, বছর: 2021
}
```

### টেক্সট প্রতিস্থাপন
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 was updated in $2");

    println!("আপডেটেড টেক্সট: {}", replaced);
    // আউটপুট: আপডেটেড টেক্সট: Rust was updated in 2023, C++ was updated in 2022, Python was updated in 2021
}
```

### একটি রেজেক্স ব্যবহার করে টেক্সট বিভক্তি করা
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // যেকোনো অ-শব্দাংশ চরিত্রে বিভাজিত
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("ভাষা: {}", field);
    }
    // আউটপুট:
    // ভাষা: Rust
    // ভাষা: C++
    // ভাষা: Python
    // ভাষা: Go
}
```

এই উদাহরণগুলি Rustে নিয়মিত প্রকাশের সাথে কাজ শুরু করার জন্য একটি প্রাথমিক গাইড প্রদান করে। আপনার প্রয়োজনীয়তা যত জটিল হয়, `regex` ক্রেট জটিল প্যাটার্ন মিলন এবং টেক্সট পরিচালনা কাজে বিপুল ফাংশনালিটি অফার করে।
