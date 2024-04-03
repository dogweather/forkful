---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:29.886588-06:00
description: "\u09AB\u09BE\u0982\u09B6\u09A8\u09C7 \u0995\u09CB\u09A1 \u0986\u09AF\
  \u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u0995\u09C7 \u09AA\u09C1\u09A8\u09B0\u09BE\u09AF\u09BC \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0\u09AF\u09CB\u0997\u09CD\u09AF, \u09AE\u09A1\u09BF\
  \u0989\u09B2\u09BE\u09B0 \u0985\u0982\u09B6\u09C7 \u09AD\u09BE\u0997 \u0995\u09B0\
  \u09BE \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09A8\u09BE\u09AE \u09A6\u09CD\u09AC\
  \u09BE\u09B0\u09BE \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4\u0964 \u0986\u09AE\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0986\u09AE\u09BE\u09A6\u09C7\
  \u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u0986\u09B0\u0993\u2026"
lastmod: '2024-03-17T18:47:43.814987-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BE\u0982\u09B6\u09A8\u09C7 \u0995\u09CB\u09A1 \u0986\u09AF\u09BC\
  \u09CB\u099C\u09A8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u0995\u09C7 \u09AA\u09C1\u09A8\u09B0\u09BE\u09AF\u09BC \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0\u09AF\u09CB\u0997\u09CD\u09AF, \u09AE\u09A1\u09BF\u0989\
  \u09B2\u09BE\u09B0 \u0985\u0982\u09B6\u09C7 \u09AD\u09BE\u0997 \u0995\u09B0\u09BE\
  \ \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09A8\u09BE\u09AE \u09A6\u09CD\u09AC\u09BE\
  \u09B0\u09BE \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4\u0964 \u0986\u09AE\u09B0\
  \u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\
  \ \u0995\u09CB\u09A1\u0995\u09C7 \u0986\u09B0\u0993 \u09AA\u09B0\u09BF\u09B7\u09CD\
  \u0995\u09BE\u09B0, \u09AA\u09A0\u09A8\u09C0\u09AF\u09BC \u098F\u09AC\u0982 \u09A1\
  \u09BF\u09AC\u09BE\u0997 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u098F\u099F\u09BF \u09A8\u09BF\u099C\u09C7\
  \u09A6\u09C7\u09B0\u0995\u09C7 \u09AA\u09C1\u09A8\u09B0\u09BE\u09AC\u09C3\u09A4\u09CD\
  \u09A4\u09BF \u09A8\u09BE \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u0986\u09AA\u09A1\
  \u09C7\u099F \u09AA\u09CD\u09B0\u09AC\u09BE\u09B9\u09BF\u09A4 \u0995\u09B0\u09BE\
  \u09B0 \u09AC\u09CD\u09AF\u09BE\u09AA\u09BE\u09B0\u09C7\u0964."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\u0982\u0997\u09A0\u09A8 \u0995\u09B0\u09BE"
weight: 18
---

## কিভাবে:
ধরুন, আপনার কাছে এমন কোড আছে যা একাধিকবার একটি বৃত্তের অঞ্চল গণনা করে। সূত্রটি পুনরাবৃত্তি না করে, এটিকে একটি ফাংশনে আবদ্ধ করুন।

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("বৃত্তের অঞ্চল হল: {}", area);
}
```

আউটপুট:

```
বৃত্তের অঞ্চল হল: 78.53981633974483
```

## গভীরে ডুব:
ঐতিহাসিকভাবে, ফাংশন গণিত থেকে এসেছিল, যেখানে তারা ইনপুটকে আউটপুটে ম্যাপ করে। কোডিংয়ে, তারা অ্যাসেম্বলির দিনগুলিতে থেকেছে, যদিও আমরা তাদের 'সাবরুটিন' বলেছিলাম। রাস্ট ফাংশনে মান এবং অন্য ফাংশনে ফেরত দিতে পারে ধন্যবাদ প্রথম শ্রেণির ফাংশন এবং ক্লোজারের কারণে।

বিকল্প? ইনলাইন কোড অথবা ম্যাক্রো, কিন্তু তারা জটিল লজিকের জন্য অগোছালো। অবজেক্ট সহ পদ্ধতিগুলি আরেকটি উপায় কার্যকারিতা সংগঠিত করার, স্ট্যান্ডালোন ফাংশনের থেকে আলাদা স্বাদ।

রাস্টে বাস্তবায়ন বেশ সোজা। ফাংশনগুলি তাদের প্যারামিটারের ধরন এবং ফেরতের ধরন ঘোষণা করে। তাদের অনুসরণের প্রথা হল 'স্নেক কেস' নেমিং। আপনার কাছে আপনার পাবলিক ফাংশন (`pub fn`) আছে মডিউলের বাইরে ব্যবহারের জন্য এবং ব্যক্তিগতগুলি অভ্যন্তরীণ ব্যবহারের জন্য। এবং রাস্টে এই দুর্দান্ত বৈশিষ্ট্য আছে যেখানে আপনার ফাংশনের শেষ প্রকাশের জন্য একটি `return` কীওয়ার্ডের প্রয়োজন হয় না।

## আরো দেখুন
আরও তথ্যের জন্য এগুলি দেখুন:
- রাস্ট প্রোগ্রামিং ভাষা বই: [ফাংশনস](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- রাস্ট উদাহরণ অনুযায়ী [ফাংশনস](https://doc.rust-lang.org/rust-by-example/fn.html)
