---
title:                "একটি ওয়েবপেজ ডাউনলোড করা"
date:                  2024-03-17T17:47:37.656213-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

ওয়েব পেজ ডাউনলোড করা মানে এর মধ্যে থাকা তথ্য সংগ্রহ করা। প্রোগ্রামাররা তথ্য পেতে, পরীক্ষা স্বয়ংক্রিয় করতে, ডেটা স্ক্র্যাপ করতে, অথবা সাইটের উপলব্ধতা পরীক্ষা করতে এটি করে থাকেন।

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
