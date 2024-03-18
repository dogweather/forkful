---
title:                "বেসিক অথেন্টিকেশন সহ HTTP রিকুয়েস্ট প্রেরণ"
date:                  2024-03-17T18:19:43.937915-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
HTTP অনুরোধে মৌলিক প্রমাণীকরণের সাথে পাঠানো মানে হল একজন ব্যবহারকারী এবং পাসওয়ার্ডকে একটি অনুরোধ হেডারে সংযুক্তি করা, যাতে প্রমাণ করা যায় যে আপনি প্রবেশাধিকার পেতে অনুমোদিত। আমরা এটি করি যখন সেবাগুলি নিশ্চিত হতে চায় যে আপনি, কোনো জো স্কো নয়, জিনিস অ্যাক্সেস করার চেষ্টা করছেন।

## কিভাবে:

প্রথমে, আপনার `Cargo.toml`-এ প্রয়োজনীয় ক্রেট যুক্ত করুন:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13"
```

এখন, মৌলিক প্রমাণীকরণের সাথে একটি GET অনুরোধ পাঠানোর জন্য রাস্ট কোডটি এখানে:

```rust
use reqwest::header::{Authorization, Basic};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let user = "Aladdin";
    let password = "open sesame";
    
    let auth = Basic {
        username: user.into(),
        password: Some(password.into()),
    };
    
    let response = client
        .get("http://example.com/secrets")
        .header(Authorization(auth))
        .send()
        .await?;
    
    let content = response.text().await?;
    println!("Response: {}", content);
    
    Ok(())
}
```

যদি সব ঠিক থাকে, এটি গোপনীয়তাগুলি প্রিন্ট করবে। আপনি ধারণা পেয়ে যাবেন।

## গভীর অনুসন্ধান

`reqwest` এর আগে, আপনি মানুষকে `curl`-এ রাস্টের সাথে কুস্তি করতে দেখতেন। এটি যেন হ্যান্ডসো এর বিপরীতে চেইনসো পছন্দ করা। মৌলিক প্রমাণীকরণ, যদিও খুব সহজ, ফোর্ট নক্স নয়। এটি শুধুমাত্র "username:password" এর Base64 – কোনো এনক্রিপশন নেই, তাই HTTPS একটি অবশ্যই।

বিকল্প? OAuth 2.0 মৌলিক এর চারিদিকে ঘুরে বেড়ায়, স্পর্শযোগ্য প্রমাণীকরণের পরিবর্তে টোকেন অফার করে। তবে, এটি জটিল। তারপরে আছে বিয়ারার প্রমাণীকরণ, গোপন হাতমিলানের মতো টোকেন ধারণ করে।

অভ্যন্তরে, `reqwest` একটি উচ্চ-স্তরের HTTP ক্লায়েন্ট যা রাস্টের অ্যাসিঙ্ক বৈশিষ্ট্যের সাথে ভালোভাবে মিলে যায়। 'Basic' কাঠামোটি হেডার তৈরি করে, 'Authorization' এটি ভেতরে ঢোকায়, এবং প্রেস্তো, আপনি একটি গোপন ফিসফিসানি দিয়ে সার্ভারের দরজায় কড়া নাড়ছেন।

## আরও দেখুন

আরও লোর এবং জাদুকরীর জন্য:

- reqwest ডকুমেন্টেশন: [https://docs.rs/reqwest](https://docs.rs/reqwest)
- HTTP বেসিক অ্যাক্সেস প্রমাণীকরণ বুঝতে: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- রাস্ট অ্যাসিঙ্ক প্রোগ্রামিং: [https://rust-lang.github.io/async-book/](https://rust-lang.github.io/async-book/)
- rust base64 ক্রেট ডকুমেন্টেশন: [https://docs.rs/base64](https://docs.rs/base64)
