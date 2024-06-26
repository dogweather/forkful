---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:06.724462-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 Rust\
  \ \u09A6\u09BF\u09DF\u09C7 \u0986\u09AE\u09BE\u09A6\u09C7\u09B0 \u09B9\u09BE\u09A4\
  \ \u09A8\u09CB\u0982\u09B0\u09BE \u0995\u09B0\u09BF\u0964 \u09A7\u09B0\u09C1\u09A8\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0986\u099B\u09C7, \u098F\u09AC\u0982\
  \ \u0986\u09AA\u09A8\u09BF \u098F\u09B0 \u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u0982\u09B6 \u09A8\u09BF\u09A4\u09C7\
  \ \u099A\u09BE\u0987\u099B\u09C7\u09A8\u0964 \u0986\u09AA\u09A8\u09BF \u09B8\u09CD\
  \u09B2\u09BE\u0987\u09B8\u09BF\u0982\u2026"
lastmod: '2024-03-17T18:47:43.797466-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 Rust \u09A6\u09BF\u09DF\u09C7 \u0986\u09AE\u09BE\
  \u09A6\u09C7\u09B0 \u09B9\u09BE\u09A4 \u09A8\u09CB\u0982\u09B0\u09BE \u0995\u09B0\
  \u09BF\u0964 \u09A7\u09B0\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\
  \u099B\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0986\u099B\u09C7, \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF \u098F\u09B0 \u098F\
  \u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\
  \u0982\u09B6 \u09A8\u09BF\u09A4\u09C7 \u099A\u09BE\u0987\u099B\u09C7\u09A8\u0964\
  \ \u0986\u09AA\u09A8\u09BF \u09B8\u09CD\u09B2\u09BE\u0987\u09B8\u09BF\u0982 `&str[start..end]`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8 \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 `start` \u09B9\u09B2\u09CB\
  \ \u0986\u09AA\u09A8\u09BF \u09AF\u09C7\u0996\u09BE\u09A8 \u09A5\u09C7\u0995\u09C7\
  \ \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09A4\u09C7 \u099A\u09BE\u09A8, \u098F\u09AC\
  \u0982 `end` \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\u09BF \u09AF\u09C7\u0996\u09BE\
  \u09A8\u09C7 \u09B6\u09C7\u09B7 \u0995\u09B0\u09A4\u09C7 \u099A\u09BE\u09A8\u0964\
  ."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কীভাবে:
চলুন Rust দিয়ে আমাদের হাত নোংরা করি। ধরুন আপনার কাছে একটি স্ট্রিং আছে, এবং আপনি এর একটি নির্দিষ্ট অংশ নিতে চাইছেন। আপনি স্লাইসিং `&str[start..end]` ব্যবহার করতে পারেন যেখানে `start` হলো আপনি যেখান থেকে শুরু করতে চান, এবং `end` হলো আপনি যেখানে শেষ করতে চান।

```Rust
fn main() {
    let text = "The quick brown fox jumps over the lazy dog";
    let quick_brown = &text[4..15]; // ৪র্থ থেকে ১৪তম অনুক্রমিকে স্লাইস
    println!("{}", quick_brown); // প্রদর্শন করে: quick brown
}
```

স্লাইসিং পরিচ্ছন্ন কিন্তু যদি আপনার অনুক্রমিক চরিত্র সীমানার উপর না পড়ে তাহলে এটি প্যানিকের সৃষ্টি করতে পারে। এটি প্রতিরোধ করতে, Rust `get` মেথড যেমন প্রদান করে:

```Rust
fn main() {
    let text = "The quick brown fox";
    match text.get(4..15) {
        Some(substring) => println!("{}", substring), // নিরাপদ স্লাইসিং
        None => println!("Slice is out of bounds."),
    }
}

// প্রদর্শন করে: quick brown
```

এতেই আপনি Rust-এ সাবস্ট্রিং এক্সট্রাকশনের একটি দ্রুত চাক্ষুষ করলেন। দেখুন কিভাবে সহজ ছিল!

## গভীর ডাইভ
UTF-8 এনকোডেড সিরিয়ালে যেমন Rust ভাষাগুলিতে স্লাইসিং একটু জটিল — চরিত্রগুলি একাধিক বাইট হতে পারে! Rust এর আগে, C ভাষার মতো ভাষাগুলিতে, স্ট্রিং হ্যান্ডলিং ম্যানুয়ালি মেমোরি পরিচালনা করা অপরাধপূর্ণ মাথাব্যথার কারণ হত।

Rust-এর `str` টাইপ হলো UTF-8 বাইটের একটি ক্রম, সবসময় বৈধ UTF-8। সাবস্ট্রিং নিরাপদে এক্সট্রাক্ট করা এই চরিত্র সীমানাগুলোকে সম্মান করে।

স্লাইসিং ছাড়া বিকল্পের মধ্যে অন্তর্ভুক্ত আছে ইটারেটর বা রেগেক্স ব্যবহার করে আরও জটিল প্যাটার্নের জন্য, কিন্তু এগুলোর অতিরিক্ত ওভারহেড আসে। স্লাইসিং করার সময়, Rust রানটাইমে বাইট অনুক্রমিক চর সীমানার সাথে মেলে কিনা তা পরীক্ষা করে, অবৈধ স্লাইস থেকে সম্ভাব্য ক্রাশ প্রতিরোধ করে।

## আরও দেখুন
- স্ট্রিংসের উপর Rust বই: https://doc.rust-lang.org/book/ch08-02-strings.html
- স্ট্রিংস– Rust by Example: https://doc.rust-lang.org/rust-by-example/std/str.html
- `str` এর জন্য Rust ডকস: https://doc.rust-lang.org/std/primitive.str.html
