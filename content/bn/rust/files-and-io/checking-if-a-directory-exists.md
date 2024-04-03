---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:34.750508-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  \u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF (`std`) \u09A1\u09BF\
  \u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\
  \u09BE\u09A8\u09A4\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0\
  \ \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09BE\u09B0\u09BF\u09A4\u09BE \u09B8\u09B0\
  \u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7, `std::path::Path` \u098F\u09AC\u0982\
  \ `std::fs` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7\u0964\u2026"
lastmod: '2024-03-17T18:47:43.824550-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF (`std`) \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\
  \u09B0 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8\u09A4\u09BE \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\
  \u09BE\u09B0\u09BF\u09A4\u09BE \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\
  \u09C7, `std::path::Path` \u098F\u09AC\u0982 `std::fs` \u09AE\u09A1\u09BF\u0989\u09B2\
  \u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u09B0\u09BE\u09B8\u09CD\u099F\u09C7\u09B0 \u09AE\u09BE\u09A8\u0995\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\
  \u099C \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u09DF\u09BE \u09B9\u09B2\
  ."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
রাস্টের স্ট্যান্ডার্ড লাইব্রেরি (`std`) ডিরেক্টরির বিদ্যমানতা যাচাই করার কার্যকারিতা সরবরাহ করে, `std::path::Path` এবং `std::fs` মডিউলের মাধ্যমে। এখানে রাস্টের মানক প্রক্রিয়া ব্যবহার করে একটি সহজ উদাহরণ দেয়া হল:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("ডিরেক্টরি বিদ্যমান আছে।");
    } else {
        println!("ডিরেক্টরি বিদ্যমান নেই।");
    }
}
```

ধরুন ডিরেক্টরি বিদ্যমান থাকলে, নমুনা আউটপুট:
```
ডিরেক্টরি বিদ্যমান আছে।
```

জটিল পরিস্থিতি বা উন্নত বৈশিষ্ট্যাদি (যেমন অ্যাসিঙ্ক্রোনাস ফাইল সিস্টেম অপারেশন) জন্য, আপনি হয়তো `tokio` মতো থার্ড-পার্টি লাইব্রেরি ব্যবহার করা বিবেচনা করতে পারেন, বিশেষ করে যদি আপনি একটি অ্যাসিঙ্ক রানটাইমের মধ্যে কাজ করেন। এখানে `tokio` ব্যবহার করে একই কাজ কিভাবে করবেন তা দেখানো হল:

প্রথমে, আপনার `Cargo.toml`-এ `tokio` যোগ করুন:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

তারপর, আপনি `tokio::fs` ব্যবহার করে অ্যাসিঙ্ক্রোনাসভাবে ডিরেক্টরির বিদ্যমানতা যাচাই করুন:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("ডিরেক্টরি বিদ্যমান আছে।");
            } else {
                println!("পাথটি বিদ্যমান কিন্তু এটি ডিরেক্টরি নয়।");
            }
        },
        Err(_) => println!("ডিরেক্টরি বিদ্যমান নেই।"),
    }
}
```

ধরুন ডিরেক্টরি বিদ্যমান না থাকলে, নমুনা আউটপুট:
```
ডিরেক্টরি বিদ্যমান নেই।
```

এই উদাহরণগুলি দেখায় যে, রাস্ট এবং এর ইকোসিস্টেম সফটওয়্যার ডেভেলপমেন্টের বিভিন্ন প্রয়োজনে মিলে যাওয়ার জন্য সিঙ্ক্রোনাস ও অ্যাসিঙ্ক্রোনাস উপায় প্রদান করে।
