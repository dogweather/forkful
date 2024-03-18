---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:25.742430-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\
  \u09C7\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u0995\u09A4\u099F\u09BF\
  \ \u0985\u0995\u09CD\u09B7\u09B0 \u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09C7 \u09A4\
  \u09BE \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AD\
  \u09BE\u09B2\u09BF\u09A1\u09C7\u099F, \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  , \u0985\u09A5\u09AC\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\
  \u09BE \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09B0\u09C0\u09AD\u09BE\u09AC\u09C7\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09DF\u09BE\u099C\u09BE\u09A4\u2026"
lastmod: '2024-03-17T18:47:43.799724-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\
  \u09C7\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u0995\u09A4\u099F\u09BF\
  \ \u0985\u0995\u09CD\u09B7\u09B0 \u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09C7 \u09A4\
  \u09BE \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AD\
  \u09BE\u09B2\u09BF\u09A1\u09C7\u099F, \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  , \u0985\u09A5\u09AC\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\
  \u09BE \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09B0\u09C0\u09AD\u09BE\u09AC\u09C7\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09DF\u09BE\u099C\u09BE\u09A4\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিংয়ের দৈর্ঘ্য বের করা মানে এটি কতটি অক্ষর ধারণ করে তা গণনা করা। প্রোগ্রামাররা এটি ভালিডেট, ফরম্যাট, অথবা টেক্সট ডেটা কার্যকরীভাবে প্রক্রিয়াজাত করার জন্য করে।

## কিভাবে:
রাস্ট আপনাকে `len()` দেয় সরল দৈর্ঘ্যের জন্য:

```Rust
fn main() {
    let greeting = "Hello, world!";
    println!("Length: {}", greeting.len());
}
```

আউটপুট: `Length: 13`

তবে সাবধান, `len()` বাইট গণনা করে, অক্ষর নয়। অক্ষরের সংখ্যা জন্য, `.chars().count()` ব্যবহার করুন:

```Rust
fn main() {
    let greeting = "¡Hola, mundo!";
    println!("Character count: {}", greeting.chars().count());
}
```

আউটপুট: `Character count: 12`

## গভীরে যাওয়া
`len()` বাইট গণনা করে কারণ রাস্ট স্ট্রিংগুলি UTF-8 কোডেড। ঐতিহাসিকভাবে, প্রথম কম্পিউটারগুলি ASCII ব্যবহার করত, প্রতিটি অক্ষরকে একটি একক বাইট দিয়ে প্রতিনিধিত্ব করত। অবশ্য, UTF-8 বিপুল পরিমাণ অক্ষর সমর্থন করে, প্রতিটির জন্য 1 থেকে 4 বাইট ব্যবহার করে।

আপনি যখন `len()` কল করেন, Rust একটি স্ট্রিংয়ের মধ্যে বাইটগুলি গণনা করে, যা দ্রুত কিন্তু সবসময় অক্ষর সংখ্যার সাথে মিলবে না। উদাহরণস্বরূপ, ইমোজি বা নির্দিষ্ট অ্যাকসেন্টেড অক্ষরগুলি একাধিক বাইট নেয়। এজন্য `.chars().count()` গুরুত্বপূর্ণ—এটি অক্ষরগুলির উপর ইটারেট করে এবং ইউনিকোড স্কেলার মানের সংখ্যা দেয়, যা বেশিরভাগ মানুষ প্রকৃত অক্ষরের সংখ্যা হিসেবে আশা করে।

বিকল্প হিসাবে, `.chars().count()` সঠিক কিন্তু লম্বা স্ট্রিং জন্য ধীর কারণ এটি প্রতিটি অক্ষর দিয়ে ইটারেট করতে হয়। যদি পারফরম্যান্স জরুরি হয়, এবং আপনি ASCII অথবা স্থির-প্রস্থ ইউনিকোড অক্ষর নিয়ে নিশ্চিত থাকেন, `len()` আরও কার্যকরী।

শেষে, রাস্টের স্ট্রিং ইন্ডেক্সিং UTF-8 এনকোডিং কাজ করার কারণে অক্ষরের অবস্থান অনুসারে সরাসরি অ্যাক্সেস অনুমতি দেয় না। রাস্ট এমন অপারেশনগুলি প্রতিরোধ করে যা আকস্মিকভাবে অসম্পূর্ণ অক্ষরে স্ট্রিং ব্রেক করতে পারে বা স্লাইস করতে পারে।

## আরও দেখুন
- রাস্টের অফিসিয়াল স্ট্রিং ডকুমেন্টেশন: [https://doc.rust-lang.org/std/string/](https://doc.rust-lang.org/std/string/)
- স্ট্রিংস নিয়ে রাস্ট বুক: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- UTF-8 বনাম ASCII আরও বুঝতে, দেখুন [https://tools.ietf.org/html/rfc3629](https://tools.ietf.org/html/rfc3629)
