---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:51.229955-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\u09C7, \u09AD\u09C7\u09B0\u09BF\u09DF\
  \u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09B8\u09A8\u09CD\u09A8\u09BF\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\
  \ \u09B9\u09DF\u0964 \u098F\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A8\u09BF\u09B0\u09CD\u09AE\u09BE\u09A3 \u0995\u09B0\u09BE \u09B8\u09C1\u099A\
  \u09BE\u09B0\u09C1 \u0993 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u0995\
  \u09B0\u09C7, \u09AC\u09C7\u09A2\u09AA \u099C\u09CB\u09DC\u09BE\u09A4\u09BE\u09B2\
  \u09BF \u098F\u09DC\u09BF\u09DF\u09C7 \u09AF\u09BE\u09DF\u0964"
lastmod: '2024-03-17T18:47:43.794296-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\u09C7, \u09AD\u09C7\u09B0\u09BF\u09DF\
  \u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09B8\u09A8\u09CD\u09A8\u09BF\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\
  \ \u09B9\u09DF\u0964 \u098F\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A8\u09BF\u09B0\u09CD\u09AE\u09BE\u09A3 \u0995\u09B0\u09BE \u09B8\u09C1\u099A\
  \u09BE\u09B0\u09C1 \u0993 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u0995\
  \u09B0\u09C7, \u09AC\u09C7\u09A2\u09AA \u099C\u09CB\u09DC\u09BE\u09A4\u09BE\u09B2\
  \u09BF \u098F\u09DC\u09BF\u09DF\u09C7 \u09AF\u09BE\u09DF\u0964"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

স্ট্রিং ইন্টারপোলেশনে, ভেরিয়েবলগুলি সরাসরি স্ট্রিংয়ের মধ্যে সন্নিবেশ করা হয়। এটি স্ট্রিং নির্মাণ করা সুচারু ও পাঠযোগ্য করে, বেঢপ জোড়াতালি এড়িয়ে যায়।

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
