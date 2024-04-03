---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:51.820977-06:00
description: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\
  \u09BF\u0982 \u0995\u09B0\u09BE \u09AE\u09C2\u09B2\u09A4 HTML \u09A1\u0995\u09C1\
  \u09AE\u09C7\u09A8\u09CD\u099F \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE, \u09AF\u09BE \u0993\u09AF\u09BC\u09C7\u09AC\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA\u09BF\u0982, \u09A1\u09C7\
  \u099F\u09BE \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09B6\
  \u09A8, \u0985\u09A5\u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u0995\u09CD\u09B0\
  \u09B2\u09BE\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AE\u09BE\u09A3\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964\u2026"
lastmod: '2024-03-17T18:47:43.806824-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\
  \u09BF\u0982 \u0995\u09B0\u09BE \u09AE\u09C2\u09B2\u09A4 HTML \u09A1\u0995\u09C1\
  \u09AE\u09C7\u09A8\u09CD\u099F \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE, \u09AF\u09BE \u0993\u09AF\u09BC\u09C7\u09AC\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA\u09BF\u0982, \u09A1\u09C7\
  \u099F\u09BE \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09B6\
  \u09A8, \u0985\u09A5\u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u0995\u09CD\u09B0\
  \u09B2\u09BE\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AE\u09BE\u09A3\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u0993\u09AF\u09BC\u09C7\u09AC \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\u09CD\u09AF\
  \ \u09B8\u0982\u0997\u09CD\u09B0\u09B9 \u0985\u099F\u09CB\u09AE\u09C7\u099F \u0995\
  \u09B0\u09A4\u09C7, \u0993\u09AF\u09BC\u09C7\u09AC \u0995\u09A8\u09CD\u099F\u09C7\
  \u09A8\u09CD\u099F \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\
  \u09A4\u09C7, \u0985\u09A5\u09AC\u09BE \u098F\u0995 \u09AA\u09CD\u09B2\u09CD\u09AF\
  \u09BE\u099F\u09AB\u09B0\u09CD\u09AE \u09A5\u09C7\u0995\u09C7 \u0985\u09A8\u09CD\
  \u09AF \u09AA\u09CD\u09B2\u09CD\u09AF\u09BE\u099F\u09AB\u09B0\u09CD\u09AE\u09C7\
  \ \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AE\u09BE\u0987\u0997\u09CD\
  \u09B0\u09C7\u099F \u0995\u09B0\u09A4\u09C7 \u098F\u0987 \u0995\u09BE\u099C \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

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
