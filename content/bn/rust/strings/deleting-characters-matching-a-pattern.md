---
title:                "একটি নমুনা মেলে অক্ষরগুলি মুছে ফেলা"
date:                  2024-03-17T17:47:32.561687-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

একটি স্ট্রিংয়ে নির্দিষ্ট প্যাটার্নের সাথে মিল রেখে অক্ষর মুছে ফেলা মানে নির্দিষ্ট অক্ষরের অনুক্রম খুঁজে বের করা এবং মুছে ফেলা। প্রোগ্রামাররা টেক্সট পরিষ্কার করা, ডেটা পার্স করা, অথবা ব্যাক্তিগত বার্তা নির্দিষ্ট ফরম্যাটে সাজাতে এটি করে থাকেন।

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
