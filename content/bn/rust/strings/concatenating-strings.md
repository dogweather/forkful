---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:35.257828-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  \ \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u0997\
  \u09C1\u09B2\u09BF \u098F\u0995\u09B8\u09BE\u09A5\u09C7 \u099C\u09C1\u09A1\u09BC\
  \u09A4\u09C7 \u0995\u09AF\u09BC\u09C7\u0995\u099F\u09BF \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09A6\u09C7\u09AF\u09BC\u0964 \u0986\u09B8\u09C1\u09A8, \u09A1\u09C1\u09AC\
  \ \u09A6\u09C7\u0987\u0964."
lastmod: '2024-04-05T21:53:51.968103-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F\u0997\u09C1\u09B2\u09BF \u098F\u0995\u09B8\u09BE\u09A5\
  \u09C7 \u099C\u09C1\u09A1\u09BC\u09A4\u09C7 \u0995\u09AF\u09BC\u09C7\u0995\u099F\
  \u09BF \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u09AF\u09BC\u0964 \u0986\u09B8\
  \u09C1\u09A8, \u09A1\u09C1\u09AC \u09A6\u09C7\u0987\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
রাস্ট আপনাকে টেক্সটগুলি একসাথে জুড়তে কয়েকটি উপায় দেয়। আসুন, ডুব দেই।

### `+` অপারেটর ব্যবহার করে
```Rust
let hello = "Hello".to_string();
let world = " world!";
let result = hello + world;
println!("{}", result); // আউটপুট: Hello world!
```
`+` `" world!"` কে `"Hello"` এর সাথে যুক্ত করে, কিন্তু সাবধান, `hello` অবশ্যই একটি `String` হতে হবে, স্লাইস না।

### `format!` ম্যাক্রো
```Rust
let mood = "happy";
let message = format!("Have a {} day!", mood);
println!("{}", message); // আউটপুট: Have a happy day!
```
`format!` `println!` এর মতো, টেক্সটে ভেরিয়েবলগুলি মেশা। টেমপ্লেটগুলির জন্য অত্যন্ত উপযোগী।

### একটি স্ট্রিং এ পুশ করা
```Rust
let mut tip = "Remember to".to_string();
tip.push_str(" breathe.");
println!("{}", tip); // আউটপুট: Remember to breathe.
```
`push_str` একটি স্লাইসকে একটি `String` এর সাথে যোগ করে। এক সময়ে একটি করে বিট যোগ করার জন্য ভাল।

## গভীর ডাইভ
স্ট্রিং যোগ কোন নতুন ধারণা নয়। এটা প্রোগ্রামিং এর ভোর থেকে মানুষের মধ্যে অবস্থান করছে; শেষ পর্যন্ত, আমাদের সবসময় শব্দগুলিকে মেশানোর প্রয়োজন হয়েছে।

রাস্টে, একটি `String` হল একটি বৃদ্ধিযোগ্য, পরিবর্তনযোগ্য, স্বত্বাধীন UTF-8 এনকোডেড স্ট্রিং টাইপ। এর বিকল্পগুলির মধ্যে `&str` রয়েছে, যা একটি স্ট্রিং স্লাইস, যা একটি `String` এর ভিউ।

প্রতিটি পদ্ধতির নিজস্ব সমঝোতা আছে:

- `+` অপারেটর এক বা দুটি যোগের জন্য দ্রুত কিন্তু বাম-দিকের অপারেন্ডকে গিলে ফেলে (এটি মালিকানা নেয়)। প্রতিটি `+` এর সাথে মেমরি বরাদ্দ হয়, যা যোগ হতে পারে।

- `format!` কোন স্বামিত্বযুক্ত মানগুলি ছিনতাই করে না, যা শিষ্টাচারপূর্ণ, কিন্তু এর নমনীয়তা এবং প্রতিটি কলের জন্য বরাদ্দের কারণে এটি ধীরগতির হতে পারে। এটি আপনার সুইস আর্মি ছুরি স্ট্রিং অ্যাসেম্বলির জন্য।

- `push_str` পুনরাবৃত্ত অ্যাড-অনের জন্য দক্ষ। যদি `String` এ আরও জায়গার প্রয়োজন না হয় তবে এটি বরাদ্দ করে না।

রাস্টের মালিকানা ও ঋণ নেয়ার উপর মনোনিবেশ এর মানে এটি পাইথন বা জাভাস্ক্রিপ্টের মতো ভাষাগুলির থেকে স্ট্রিংগুলির সাথে আলাদাভাবে আচরণ করে। এই পার্থক্য মেমরি নিরাপত্তা নিশ্চিত করে কিন্তু শিখতে গেলে একটি শেখার বাঁক নিয়ে আসতে পারে।

## আরও দেখুন
গভীরভাবে জানার জন্য:
- স্ট্রিংগুলি সম্পর্কে রাস্ট বই: https://doc.rust-lang.org/book/ch08-02-strings.html
- স্ট্রিংস সম্পর্কে রাস্ট বাই এক্সাম্পল: https://doc.rust-lang.org/rust-by-example/std/str.html
- রাস্ট std::string::String API ডক্স: https://doc.rust-lang.org/std/string/struct.String.html
