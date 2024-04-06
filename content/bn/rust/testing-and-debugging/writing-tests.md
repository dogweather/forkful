---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:57.242680-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  \u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\u09B8\u09CD\
  \u099F \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\
  \ \u0987\u0989\u09A8\u09BF\u099F \u099F\u09C7\u09B8\u09CD\u099F, \u0987\u09A8\u09CD\
  \u099F\u09BF\u0997\u09CD\u09B0\u09C7\u09B6\u09A8 \u099F\u09C7\u09B8\u09CD\u099F\
  , \u098F\u09AC\u0982 \u09A1\u0995\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09C7\u09B6\
  \u09A8 \u099F\u09C7\u09B8\u09CD\u099F \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\
  \u09B0\u09C7, \u09AC\u09BE\u0987\u09B0\u09C7\u09B0 \u0995\u09CB\u09A8\u09CB \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8 \u09A8\u09C7\u0987\u0964 \u099F\u09C7\u09B8\u09CD\u099F\u2026"
lastmod: '2024-04-05T21:53:51.989547-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 \u099F\u09C7\u09B8\u09CD\u099F \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\
  \u09AF\u09BC\u09BE\u09B0\u09CD\u0995 \u0987\u0989\u09A8\u09BF\u099F \u099F\u09C7\
  \u09B8\u09CD\u099F, \u0987\u09A8\u09CD\u099F\u09BF\u0997\u09CD\u09B0\u09C7\u09B6\
  \u09A8 \u099F\u09C7\u09B8\u09CD\u099F, \u098F\u09AC\u0982 \u09A1\u0995\u09C1\u09AE\
  \u09C7\u09A8\u09CD\u099F\u09C7\u09B6\u09A8 \u099F\u09C7\u09B8\u09CD\u099F \u09B8\
  \u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7, \u09AC\u09BE\u0987\u09B0\u09C7\
  \u09B0 \u0995\u09CB\u09A8\u09CB \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09A8\u09C7\u0987\
  \u0964 \u099F\u09C7\u09B8\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF `#[test]` \u09A6\u09BF\u09AF\u09BC\u09C7 \u0985\u09A8\u09CD\u09A8\u09CB\u09A4\
  \ \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u098F\u09AC\u0982 \u098F \u09B9\u09BF\u09B8\
  \u09C7\u09AC\u09C7 \u0985\u09A8\u09CD\u09A8\u09CB\u09A4 \u0995\u09B0\u09BE \u09AF\
  \u09C7\u0995\u09CB\u09A8\u09CB \u09AB\u09BE\u0982\u09B6\u09A8 \u098F\u0995\u099F\
  \u09BF \u099F\u09C7\u09B8\u09CD\u099F \u09B9\u09BF\u09B8\u09BE\u09AC\u09C7 \u0995\
  \u09AE\u09CD\u09AA\u09BE\u0987\u09B2 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964\
  ."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
রাস্টের বিল্ট-ইন টেস্ট ফ্রেমওয়ার্ক ইউনিট টেস্ট, ইন্টিগ্রেশন টেস্ট, এবং ডকুমেন্টেশন টেস্ট সমর্থন করে, বাইরের কোনো লাইব্রেরির প্রয়োজন নেই। টেস্ট করার জন্য `#[test]` দিয়ে অন্নোত করা হয় এবং এ হিসেবে অন্নোত করা যেকোনো ফাংশন একটি টেস্ট হিসাবে কম্পাইল করা হয়।

### একটি ইউনিট টেস্ট লেখা:
একটি `tests` সাব-মডিউলের মধ্যে ইউনিট টেস্টগুলি স্থাপন করুন যা তাদের পরীক্ষার জন্য `#[cfg(test)]` দিয়ে চিহ্নিত আছে এমন মডিউলের মধ্যে যাতে কেবল টেস্টিং এর সময় তাদের কম্পাইল করা হয়।

```rust
// lib.rs অথবা main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

টেস্ট চালানো:
```shell
$ cargo test
```

আউটপুট:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (or src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### একটি ইন্টিগ্রেশন টেস্ট লেখা:
ইন্টিগ্রেশন টেস্টগুলি আপনার প্রকল্পের শীর্ষ স্তরের `tests` ডিরেক্টরির মধ্যে রাখা হয়, `src` এর পাশাপাশি। `tests` এর মধ্যে প্রতিটি `.rs` ফাইল তার নিজের পৃথক ক্রেট হিসাবে কম্পাইল করা হয়।

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### জনপ্রিয় থার্ড-পার্টি লাইব্রেরিগুলির সাথে টেস্টিং:
আরও বিশদ টেস্টিং ক্ষমতা পাওয়ার জন্য, `proptest` লাইব্রেরি ফাংশন পরীক্ষার জন্য বিস্তৃত পরিসীমার ইনপুট উৎপাদন করতে পারে।

`Cargo.toml` -এ `proptest` কে একটি ডেভ ডিপেন্ডেন্সি হিসেবে যোগ করুন:

```toml
[dev-dependencies]
proptest = "1.0"
```

একই টেস্ট চালানোর জন্য `proptest` ব্যবহার করুন যা অনেকগুলি স্বয়ংক্রিয়ভাবে উৎপন্ন ইনপুটসহ:

```rust
// inside tests/integration_test.rs or a module's #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

এটি যাচাই করে যে `add` বিস্তৃত পরিসীমার `i32` ইনপুটের জন্য প্যানিক হয় না।
