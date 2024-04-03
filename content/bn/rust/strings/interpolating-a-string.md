---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:51.229955-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Rust \u098F, \u0986\u09AE\u09B0\
  \u09BE `format!` \u09AE\u09CD\u09AF\u09BE\u0995\u09CD\u09B0\u09CB \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF."
lastmod: '2024-03-17T18:47:43.794296-06:00'
model: gpt-4-0125-preview
summary: "Rust \u098F, \u0986\u09AE\u09B0\u09BE `format!` \u09AE\u09CD\u09AF\u09BE\
  \u0995\u09CD\u09B0\u09CB \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BF."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
Rust এ, আমরা `format!` ম্যাক্রো ব্যবহার করি:

```Rust
fn main() {
    let name = "Ferris";
    let greeting = format!("Hello, {}!", name);
    println!("{}", greeting); // প্রিন্ট হবে "Hello, Ferris!"
}
```
`format!` ম্যাক্রোটি `println!` এর মতো কাজ করে, তবে এটি প্রিন্ট করার পরিবর্তে ফর্ম্যাটেড স্ট্রিং ফেরত দেয়।

## গভীরে যাওয়া
Rust স্ট্রিং ইন্টারপোলেশনের জন্য ভাষার সিনট্যাক্সের পরিবর্তে `format!` এর মতো ম্যাক্রোগুলি বেছে নিয়েছে। কেন? ম্যাক্রোগুলি শক্তিশালী এবং নমনীয়—জটিল সিনট্যাক্স ছাড়াই ভাষার কার্যকারিতা প্রসারিত করে।

ঐতিহাসিকভাবে, C এর মতো ভাষায় `sprintf` এর মতো ফাংশন ব্যবহৃত হতো, যা বেঢপ এবং ত্রুটি প্রবণ ছিল। Rust এর `format!` ম্যাক্রো নিরাপদ, সাধারণ ভুলগুলি এড়াতে সাহায্য করে।

বিকল্পগুলি আছে, যেমন `+` দিয়ে জোড়া লাগানো বা হিপ আবণ্টন এড়ানোর জন্য `format_args!` ম্যাক্রো। কিন্তু সহজতা ও স্বচ্ছতার দিক দিয়ে, `format!` হলো রাজা।

পারফরমেন্স নোট: `format!` মেমরি আবণ্টন করে। পারফরমেন্স-সমালোচনা কোডের ক্ষেত্রে, বাফারে সরাসরি লেখার মতো অন্যান্য পদ্ধতি বিবেচনা করুন।

## আরও দেখুন
- অফিশিয়াল রাস্ট ডক্স `format!` এর উপর: https://doc.rust-lang.org/std/macro.format.html
- `format!` বনাম `println!`: https://doc.rust-lang.org/book/ch01-02-hello-world.html
- ফর্ম্যাটিং উপর Rust by Example: https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
