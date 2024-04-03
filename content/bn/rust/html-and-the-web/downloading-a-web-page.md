---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:37.656213-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u098F\
  \u0996\u09A8 \u0986\u09AE\u09B0\u09BE \u09B0\u09BE\u09B8\u09CD\u099F\u09C7\u09B0\
  \ `reqwest` \u0995\u09CD\u09B0\u09C7\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\
  \u09C7\u099C \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BF, \u09AF\
  \u09BE HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C, \u0985\u09CD\u09AF\
  \u09BE\u09B8\u09BF\u0999\u09CD\u0995\u09CD\u09B0\u09CB\u09A8\u09BE\u09B8 API \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\u2026"
lastmod: '2024-03-17T18:47:43.807814-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u098F\u0996\u09A8 \u0986\u09AE\u09B0\u09BE \u09B0\
  \u09BE\u09B8\u09CD\u099F\u09C7\u09B0 `reqwest` \u0995\u09CD\u09B0\u09C7\u099F \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\u09B2\
  \u09CB\u09A1 \u0995\u09B0\u09BF, \u09AF\u09BE HTTP \u0985\u09A8\u09C1\u09B0\u09CB\
  \u09A7 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09B9\u099C, \u0985\u09CD\u09AF\u09BE\u09B8\u09BF\u0999\u09CD\u0995\u09CD\
  \u09B0\u09CB\u09A8\u09BE\u09B8 API \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\
  \u09C7\u0964\n\n\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0\
  \ `Cargo.toml`-\u098F `reqwest` \u098F\u09AC\u0982 `tokio` \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09C1\u09A8."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
চলুন এখন আমরা রাস্টের `reqwest` ক্রেট ব্যবহার করে একটি ওয়েব পেজ ডাউনলোড করি, যা HTTP অনুরোধ করার জন্য একটি সহজ, অ্যাসিঙ্ক্রোনাস API প্রদান করে।

প্রথমে, আপনার `Cargo.toml`-এ `reqwest` এবং `tokio` যোগ করুন:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

এখন, আপনার রাস্ট কোডে:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    let body = res.text().await?;
    println!("বডি:\n{}", body);

    Ok(())
}
```

নমুনা আউটপুট এরকম দেখাবে, যদিও আসল কন্টেন্ট ভিন্ন হবে:

```
বডি:
<!doctype html>
<html>
<head>
    <title>উদাহরণ ডোমেইন</title>
...
</body>
</html>
```

## গভীর আলোচনা
`reqwest` ক্রেটটি রাস্টে ওয়েব কন্টেন্ট ডাউনলোড করার সবচেয়ে সরল উপায়গুলোর অন্যতম। এটি পূর্ববর্তী HTTP লাইব্রেরিগুলো থেকে বিকশিত হয়েছে, সিঙ্ক্রোনাস এবং অ্যাসিঙ্ক্রোনাস ইন্টারফেস উভয়ই প্রদান করে।

বিকল্পগুলি অন্তর্ভুক্ত `hyper` (যা `reqwest` নিজেও ভিতরে ব্যবহার করে), অথবা রাস্টের জন্য `curl` বাইন্ডিং ব্যবহার করা।

একটি পৃষ্ঠা ডাউনলোড করার জন্য মুখ্য বাস্তবায়ন ধাপগুলি অন্তর্ভুক্ত একটি HTTP GET অনুরোধ করা এবং প্রতিক্রিয়া প্রক্রিয়া করা। `tokio` এর সাথে অ্যাসিঙ্ক্রোনাস প্রোগ্রামিং মানে আপনার অ্যাপ নেটওয়ার্ক অপারেশন সম্পূর্ণ হওয়ার সময় সাড়াশীল থাকে।

## আরও দেখুন:
- [`reqwest` ডকুমেন্টেশন](https://docs.rs/reqwest/)
- [`tokio` ডকুমেন্টেশন](https://docs.rs/tokio/)
- [রাস্ট `async`/`await` বই](https://rust-lang.github.io/async-book/)
- [HTTP সম্পর্কে MDN ওয়েব ডক্স](https://developer.mozilla.org/en-US/docs/Web/HTTP)
