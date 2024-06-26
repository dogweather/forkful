---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:55.872863-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u0996\u09A8\u09CB \u0995\
  \u0996\u09A8\u09CB \u0986\u09AA\u09A8\u09BF \u098F\u09AE\u09A8 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AA\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8 \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09AE\u09BF\u09B6\
  \u09CD\u09B0 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u099A\u09BF\u09B9\u09CD\
  \u09A8 \u09A5\u09BE\u0995\u09C7, \u09AF\u09C7\u09AE\u09A8."
lastmod: '2024-04-05T21:53:51.961199-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u0996\u09A8\u09CB \u0995\u0996\u09A8\u09CB \u0986\u09AA\u09A8\u09BF\
  \ \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09AA\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u09AF\u09C7\u0996\
  \u09BE\u09A8\u09C7 \u09AE\u09BF\u09B6\u09CD\u09B0 \u0989\u09A6\u09CD\u09A7\u09C3\
  \u09A4\u09BF \u099A\u09BF\u09B9\u09CD\u09A8 \u09A5\u09BE\u0995\u09C7, \u09AF\u09C7\
  \u09AE\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hello, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // আউটপুট: Hello, Rustaceans!
}
```

কখনো কখনো আপনি এমন একটি স্ট্রিং পেতে পারেন যেখানে মিশ্র উদ্ধৃতি চিহ্ন থাকে, যেমন:

```Rust
fn main() {
    let mixed_quoted = "'Rust says: \"Hello, World!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // আউটপুট: Rust says: "Hello, World!"
}
```

এখানে, কেবল বাহ্যিকতম একক উদ্ধৃতি চিহ্নগুলি অপসারিত হয়।

## গভীর ডাইভ
একটি স্ট্রিং থেকে উদ্ধৃতি চিহ্ন অপসারণের সময়, আপনি ভাবতে পারেন যে এটি কেন `.replace("\"", "")` দ্বারা সহজভাবে সমাধান করা হয় না। প্রাথমিকভাবে, টেক্সট নিয়ে কাজ করা কম মানকীকৃত ছিল এবং বিভিন্ন সিস্টেমে বিভিন্নভাবে টেক্সট সংরক্ষণ এবং সম্প্রেষণের পদ্ধতি ছিল, প্রায়ই বিশেষ অক্ষরের জন্য কিছু 'এস্কেপ সিকোয়েন্স' দিয়ে। Rust's `trim_matches` পদ্ধতিটি বিভিন্ন অক্ষর ছাঁটাই করা এবং স্ট্রিংয়ের প্রারম্ভ (prefix), শেষ (suffix), অথবা উভয় পাশ থেকে ছাঁটাই করার ক্ষেত্রে বেশি নমনীয় হয়ে ওঠে।

অবশ্যই, বিকল্পগুলি আছে। Regex হলো স্ট্রিং ম্যানিপুলেশনের জন্য শক্তিশালী যন্ত্র, যা জটিল প্যাটার্নগুলিকে মিলিত করতে সক্ষম, এবং কেবল উদ্ধৃতি চিহ্ন অপসারণের জন্য অতিমাত্রায় প্রয়োগ হতে পারে। `trim_in_place` এর মতো লাইব্রেরিগুলি একটি নতুন `String` বস্তু তৈরির ওভারহেড ছাড়াই স্থানে ছাঁটাই প্রদান করতে পারে, যা পারফরম্যান্স-সমালোচনা অ্যাপ্লিকেশনগুলির জন্য কাঙ্খিত হতে পারে।

আসলে, `trim_matches` পদ্ধতিটি দ্বি-পাশ থেকে স্ট্রিংয়ের অক্ষরগুলিকে ঘুরিয়ে দেখে, প্রদত্ত প্যাটার্নের বিরুদ্ধে যাচাই করে এবং অমিল অক্ষর পাওয়া যাওয়া পর্যন্ত চলতে থাকে। এটি যা করে তা জন্য দক্ষ, তবে সর্বদা সচেতন থাকুন যে এটি ইউনিকোড স্কেলার মানগুলি নিয়ে কাজ করে। আপনার স্ট্রিং যদি মাল্টি-বাইট ইউনিকোড অক্ষর ধারণ করে তবে এটি তাদের ভেঙে না ফেলার সম্পর্কে চিন্তিত হবেন না।

## আরও দেখুন
- Rust এর স্ট্রিং ম্যানিপুলেশনের উপর নথি: https://doc.rust-lang.org/book/ch08-02-strings.html
- জটিল প্যাটার্নের জন্য `regex` ক্রেট: https://crates.io/crates/regex
- প্রাকটিক্যাল কোডিং সিনারিওর জন্য Rust by Example: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
