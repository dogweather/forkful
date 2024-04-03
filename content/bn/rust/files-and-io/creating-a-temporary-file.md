---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:18.564089-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Rust-\u098F, `tempfile` \u0995\
  \u09CD\u09B0\u09C7\u099F\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09AC\u09BF\u09B7\u09AF\u09BC\u0995 \u0995\u09BE\u09B0\
  \u09CD\u09AF\u09BE\u09AC\u09B2\u09C0\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\
  \u099F\u09BF \u09AD\u09BE\u09B2 \u09AC\u09A8\u09CD\u09A7\u09C1\u0964 \u098F\u099F\
  \u09BF \u0986\u09AA\u09A8\u09BE\u09B0 `Cargo.toml`-\u098F \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09C1\u09A8."
lastmod: '2024-03-17T18:47:43.830281-06:00'
model: gpt-4-0125-preview
summary: "Rust-\u098F, `tempfile` \u0995\u09CD\u09B0\u09C7\u099F\u099F\u09BF \u0985\
  \u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09AC\u09BF\
  \u09B7\u09AF\u09BC\u0995 \u0995\u09BE\u09B0\u09CD\u09AF\u09BE\u09AC\u09B2\u09C0\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AD\u09BE\u09B2 \u09AC\u09A8\
  \u09CD\u09A7\u09C1\u0964 \u098F\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 `Cargo.toml`-\u098F\
  \ \u09AF\u09CB\u0997 \u0995\u09B0\u09C1\u09A8."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
Rust-এ, `tempfile` ক্রেটটি অস্থায়ী ফাইল বিষয়ক কার্যাবলীর জন্য একটি ভাল বন্ধু। এটি আপনার `Cargo.toml`-এ যোগ করুন:

```toml
[dependencies]
tempfile = "3.3.0"
```

তারপর, আপনি এভাবে একটি অস্থায়ী ফাইল তৈরি করতে পারেন:

```rust
use tempfile::NamedTempFile;
use std::io::{Write, Read};

fn main() -> std::io::Result<()> {
    let mut temp_file = NamedTempFile::new()?;
    write!(temp_file, "Hello, world!")?;

    let mut content = String::new();
    temp_file.reopen()?.read_to_string(&mut content)?;
    println!("Temp file contains: {}", content);

    // `temp_file` যখন স্কোপের বাইরে যায় তখন অস্থায়ী ফাইলটি মুছে যায়
    Ok(())
}
```

কোডটি চালান। জাদু ঘটে। একটি ফাইল দেখা দেয়, তারপর ফুস্—শেষ হলেই চলে যায়।

## গভীর ডুব
ঐতিহাসিকভাবে, অস্থায়ী ফাইলগুলি কম্পিউটিংয়ের পুরানো পাহাড়ের মতো। দীর্ঘমেয়াদি সংরক্ষণের প্রয়োজন না হলে ডেটা সম্পাদনার একটি সাধারণ কিন্তু কার্যকরী উপায় এগুলি সবসময়ই ছিল। Rust-এর দুনিয়ায়, `tempfile` ক্রেটটি অস্থায়ী ফাইল প্রক্রিয়াকরণের প্রক্রিয়াকে মসৃণ করে, আর দরকার না হলে স্বয়ংক্রিয়ভাবে ফাইলগুলিকে পরিষ্কার করে, ম্যানুয়াল পরিষ্কারের পুরানো মাথাব্যথা এড়ানো যায়।

বিকল্প? অবশ্যই, আপনি `std::fs` এবং ম্যানুয়াল পরিষ্কারের সাথে নিজের সমাধান তৈরি করতে পারেন, কিন্তু চাকা পুনরায় আবিষ্কার করা কেন?

বিস্তারিত সম্পর্কে কি? `tempfile` অপারেটিং সিস্টেমের নির্ধারিত অস্থায়ী ডিরেক্টরিতে ফাইলগুলি তৈরি করে, এবং ফাইলের নামগুলি সংঘর্ষ এড়াতে ও নিরাপত্তা বাড়াতে হটকারি করা হয়।

## আরো দেখুন
- Rust `tempfile` ডকুমেন্টেশন: [https://docs.rs/tempfile/](https://docs.rs/tempfile/)
- Rust স্ট্যান্ডার্ড লাইব্রেরি I/O: [https://doc.rust-lang.org/std/io/](https://doc.rust-lang.org/std/io/)
- সাধারণ অস্থায়ী ফাইল ধারণা: [https://en.wikipedia.org/wiki/Temporary_file](https://en.wikipedia.org/wiki/Temporary_file)
