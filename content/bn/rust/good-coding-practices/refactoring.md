---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:12:31.933163-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u098F\
  \u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09B0\u09BE\u09B8\u09CD\
  \u099F \u0995\u09CB\u09A1 \u0995\u09C7 \u0986\u09B0\u09CB \u0987\u09A1\u09BF\u0993\
  \u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0995 \u098F\u09AC\u0982 \u09B0\u0995\u09CD\
  \u09B7\u09A3\u09AF\u09CB\u0997\u09CD\u09AF \u0995\u09B0\u09C7 \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09A4\u09C7 \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\
  \u099F\u09B0 \u0995\u09B0\u09BF\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u0995\u099F\
  \u09BF \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BF \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09AA\
  \u09C2\u09B0\u09CD\u09A3\u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.818160-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3 \u09B0\u09BE\u09B8\u09CD\u099F \u0995\u09CB\u09A1 \u0995\u09C7 \u0986\
  \u09B0\u09CB \u0987\u09A1\u09BF\u0993\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0995\
  \ \u098F\u09AC\u0982 \u09B0\u0995\u09CD\u09B7\u09A3\u09AF\u09CB\u0997\u09CD\u09AF\
  \ \u0995\u09B0\u09C7 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7 \u09B0\u09BF\
  \u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0 \u0995\u09B0\u09BF\u0964 \u0986\
  \u09AE\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BF \u09AF\
  \u09BE \u098F\u0995\u099F\u09BF \u09AA\u09C2\u09B0\u09CD\u09A3\u09B8\u0982\u0996\
  \u09CD\u09AF\u09BE\u09B0 \u09AD\u09C7\u0995\u09CD\u099F\u09B0\u09C7\u09B0 \u09B8\
  \u09AE\u09B7\u09CD\u099F\u09BF \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09C7."
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কীভাবে:
চলুন একটি সাধারণ রাস্ট কোড কে আরো ইডিওম্যাটিক এবং রক্ষণযোগ্য করে তৈরি করতে রিফ্যাক্টর করি। আমরা একটি ফাংশনের সাথে শুরু করি যা একটি পূর্ণসংখ্যার ভেক্টরের সমষ্টি গণনা করে:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

আউটপুট:
```
The sum is 15
```

এখন, চলুন এটি ইডিওম্যাটিক রাস্ট ব্যবহার করে রিফ্যাক্টর করি যেখানে ইটারেটরস এবং `fold` মেথড ব্যবহার করব:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

আউটপুটে কোনো পরিবর্তন নেই—এটি এখনো `15`—কিন্তু রিফ্যাক্টর করা সংস্করণটি আরও পরিষ্কার এবং রাস্টের শক্তি যেমন ধার গ্রহণ এবং ইটারেটর মেথডস ব্যবহার করে।

## গভীর ডুব
রিফ্যাক্টরিং এর উৎস Smalltalk সম্প্রদায়ে এবং Martin Fowler's বই "Refactoring: Improving the Design of Existing Code" এর মাধ্যমে Java দুনিয়ায় জনপ্রিয় হয়েছে। এর নীতিগুলি সার্বজনীন এবং রাস্টের ক্ষেত্রেও প্রযোজ্য, যেখানে নিরাপত্তা এবং সমান্তরালতা অপরিহার্য। রাস্ট সময়ে সময়ে জটিল সমস্যা ধরে রোবাস্ট কোড লেখাকে উৎসাহিত করে, সুতরাং রিফ্যাক্টরিং এর সময়, রাস্ট কম্পাইলার একটি নিরাপত্তা জাল হিসেবে কাজ করে।

ম্যানুয়াল রিফ্যাক্টরিং এর বিকল্প হিসাবে অটোমেটেড টুলস, যেমন 'rustfmt' কোড ফর্ম্যাটিং এর জন্য এবং 'clippy' লিন্টিং এর জন্য, যা কোড লেখার আরও ইডিওম্যাটিক উপায় সুপারিশ করতে পারে। তবে, গভীর রিফ্যাক্টরিং প্রায়ই কোডের নকশা সম্পর্কে একটি চিন্তাশীল বোঝাপড়া দাবি করে, যা এই টুলগুলি পুরোপুরি অটোমেট করতে পারে না।

রাস্টে, রিফ্যাক্টরিং হয়তো ডেটা টাইপ ব্যবহার উন্নতি, জীবনকাল কার্যকরভাবে ব্যবহার, অনাবশ্যক অ্যালোকেশন হ্রাস, অথবা প্রয়োজনীয় সময়ে `Arc<Mutex<T>>` ব্যবহার করে সমান্তরাল নকশা প্যাটার্ন ব্যবহার করা নিয়ে কেন্দ্রিক হতে পারে। `unwrap()` থেকে `Result<T, E>` এর মত আরও প্রকাশক ত্রুটি হ্যান্ডলিং এ যাওয়াও সাধারণ।

## আরও দেখুন
রাস্টে রিফ্যাক্টরিং এর আরও গভীরে যেতে:

- দ্য রাস্ট বই: https://doc.rust-lang.org/book/
- রাস্ট বাই এক্সাম্পল: https://doc.rust-lang.org/rust-by-example/
- ক্লিপি, একটি রাস্ট লিন্টিং টুল: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" মার্টিন ফাউলার দ্বারা: https://martinfowler.com/books/refactoring.html
