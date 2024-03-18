---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:04:51.820977-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

রাস্টে HTML পার্সিং করা মূলত HTML ডকুমেন্ট থেকে ডেটা বের করা, যা ওয়েব স্ক্র্যাপিং, ডেটা এক্সট্র্যাকশন, অথবা ওয়েব ক্রলার নির্মাণের জন্য অপরিহার্য। প্রোগ্রামাররা ওয়েব থেকে তথ্য সংগ্রহ অটোমেট করতে, ওয়েব কন্টেন্ট বিশ্লেষণ করতে, অথবা এক প্ল্যাটফর্ম থেকে অন্য প্ল্যাটফর্মে কন্টেন্ট মাইগ্রেট করতে এই কাজ করে থাকেন।

## কিভাবে:

রাস্টে HTML পার্স করার জন্য, আপনি প্রায়শই `scraper` ক্রেট ব্যবহার করবেন, যা HTML ডকুমেন্টস ট্রাভার্স এবং ম্যানিপুলেট করার জন্য একটি উচ্চ স্তরের ইন্টারফেস প্রদান করে।

প্রথমে, আপনার `Cargo.toml`-এ `scraper` যোগ করুন:

```toml
[dependencies]
scraper = "0.12.0"
```

এরপর, একটি সাধারণ উদাহরণ যা একটি নির্দিষ্ট HTML স্ট্রিং থেকে সমস্ত লিঙ্ক URL বের করে আনে:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Link 1</a>
        <a href="http://example.com/2">Link 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("লিঙ্ক পাওয়া গেছে: {}", link);
    }
}
```

আউটপুট:

```
লিঙ্ক পাওয়া গেছে: http://example.com/1
লিঙ্ক পাওয়া গেছে: http://example.com/2
```

এই উদাহরণে, আমরা একটি সাধারণ HTML ডকুমেন্ট পার্স করি সমস্ত `<a>` এলিমেন্টস খুঁজে বের করার জন্য এবং তাদের `href` এট্রিবিউটস এক্সট্র্যাক্ট করি, মূলত ডকুমেন্টের সব লিঙ্কের URL প্রিন্ট করে। `scraper` লাইব্রেরি CSS সিলেক্টর ব্যবহার করে নির্দিষ্ট এলিমেন্টস নির্বাচন এবং HTML পার্সিং সহজ করে তোলে, যা রাস্টে ওয়েব স্ক্র্যাপিং টাস্কসের জন্য একটি উপযুক্ত সমাধান।
