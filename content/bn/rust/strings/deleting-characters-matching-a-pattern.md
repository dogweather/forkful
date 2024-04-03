---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:32.561687-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Rust-\u098F, \u0986\u09AE\u09B0\
  \u09BE `String` \u099F\u09BE\u0987\u09AA\u09C7\u09B0 `replace` \u09AE\u09C7\u09A5\
  \u09A1 \u0985\u09A5\u09AC\u09BE \u099C\u099F\u09BF\u09B2 \u09AA\u09CD\u09AF\u09BE\
  \u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF regex \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09BF\u0964 \u098F\u09AD\u09BE\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u099F\
  \u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
lastmod: '2024-03-17T18:47:43.792319-06:00'
model: gpt-4-0125-preview
summary: "Rust-\u098F, \u0986\u09AE\u09B0\u09BE `String` \u099F\u09BE\u0987\u09AA\u09C7\
  \u09B0 `replace` \u09AE\u09C7\u09A5\u09A1 \u0985\u09A5\u09AC\u09BE \u099C\u099F\u09BF\
  \u09B2 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF regex \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09BF\u0964 \u098F\u09AD\u09BE\u09AC\u09C7 \u0986\
  \u09AA\u09A8\u09BF \u098F\u099F\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
Rust-এ, আমরা `String` টাইপের `replace` মেথড অথবা জটিল প্যাটার্নের জন্য regex ব্যবহার করতে পারি। এভাবে আপনি এটি করতে পারেন:

```rust
fn main() {
    let phrase = "Hello, _world_! -- Programming in Rust --".to_string();
    // আন্ডারস্কোরগুলোকে কিছু না দিয়ে প্রতিস্থাপন করুন
    let cleaned = phrase.replace("_", "");
    println!("{}", cleaned);

    // জটিল প্যাটার্নের জন্য regex ব্যবহার করুন (Cargo.toml এ regex ক্রেট যোগ করতে মনে রাখুন)
    let regex = regex::Regex::new(r"--.*?--").unwrap();
    let s = regex.replace_all(&cleaned, "");
    println!("{}", s);
}

// আউটপুট:
// Hello, world! -- Programming in Rust --
// Hello, world!
```

## গভীরভাবে দেখা
নির্দিষ্ট প্যাটার্নের সাথে মিল রেখে অক্ষর মুছে ফেলা শুধু Rust-এ নয়, অনেক প্রোগ্রামিং ভাষায় একটি সাধারণ অপারেশন। ঐতিহাসিকভাবে, `sed` এর মতো Unix টুলস ব্যবহার করে টেক্সটকে শক্তিশালীভাবে পরিবর্তন করা হত, এবং এখন ভাষাগুলো স্ট্রিং ম্যানিপুলেশনের জন্য built-in functions প্রদান করে।

Rust-এ, স্ট্যান্ডার্ড অ্যাপ্রোচ হল সহজ, fixed patterns-এর জন্য `replace` ব্যবহার করা। Wildcards, repeats, অথবা conditional removal-এর জন্য, আমরা regex-এ যাই। regex crate এই কাজের জন্য de facto টুল, তবে মনে রাখবেন, regex অপারেশনগুলো পারফরম্যান্সের দিক দিয়ে বেশি খরচবহুল, তাই এগুলো যথাযথভাবে ব্যবহার করুন।

Rust-এর নিরাপত্তা গ্যারান্টি টেক্সট প্রক্রিয়াজাতকরণে প্রসারিত। যেখানে কিছু ভাষায় স্ট্রিং ম্যানিপুলেশন বাফার ওভারফ্লোস এর মতো নিরাপত্তা দুর্বলতা ঘটাতে পারে, Rust-এর নকশা এই ধরণের সমস্যাগুলির বিরুদ্ধে সুরক্ষা প্রদান করে।

## আরো দেখুন
- Rust `String` ডকুমেন্টেশন: https://doc.rust-lang.org/std/string/struct.String.html 
- `regex` ক্রেট ডকুমেন্টেশন: https://docs.rs/regex/
- Rust Regex বই: https://rust-lang-nursery.github.io/regex/
