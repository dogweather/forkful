---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:33.103794-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: ."
lastmod: '2024-03-17T18:47:43.795411-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
```Rust
fn main() {
    let greeting = "HeLLo, WoRlD!";
    let lowercase_greeting = greeting.to_lowercase();
    println!("{}", lowercase_greeting); // "hello, world!"
}
```
আউটপুট:
```
hello, world!
```

## গভীর অন্বেষণ
`.to_lowercase()` মেথডের পূর্বে, আপনি হয়ত `to_ascii_lowercase()` মেথড ব্যবহার দেখেছেন, যা শুধুমাত্র ASCII অক্ষরগুলির উপর প্রভাব ফেলেছিল। Rust স্ট্যান্ডার্ড লাইব্রেরি উন্নত হয়ে, `.to_lowercase()` এর মাধ্যমে পূর্ণ Unicode সমর্থন প্রদান করেছে—অর্থাৎ এটি শুধু ইংরেজির চেয়ে বেশি কিছু ব্যবহার করতে পারে! যদি আপনার অ্যাপ্লিকেশন বহুভাষিক বিশ্বে পা রাখে, তবে এটি অনেক গুরুত্বপূর্ণ।

আসলে কি আছে? ঠিক আছে, `to_lowercase()` মেথড শুধু `A` থেকে `a` এ পরিবর্তন করে না। এটি বরং এক ধরনের ছোট ভাষাবিদ, Unicode এর পথগুলি জানা। এটি Unicode স্ট্যান্ডার্ডের অনুসরণ করে সঠিকভাবে lowercase অক্ষরগুলি পরিণত করে, তাদের সাংস্কৃতিক বৈশিষ্ট্য সম্মান করে।

অবশ্যই, বিকল্পও আছে। আপনি একটি লুপে ঢুকে, প্রতিটি char দিয়ে এটি নিজে রূপান্তর করতে পারেন। কিন্তু Rust এর স্ট্যান্ডার্ড লাইব্রেরি যখন এই কাজে পরিশ্রম করেছে, তখন নতুন করে চাকা আবিষ্কার করার কি প্রয়োজন?

## আরও দেখুন
- [Rust ডক্স উপর `to_lowercase()`](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Rust String ডক্স](https://doc.rust-lang.org/std/string/struct.String.html)
- [Unicode কেস ম্যাপিং](https://www.unicode.org/reports/tr21/tr21-5.html)
