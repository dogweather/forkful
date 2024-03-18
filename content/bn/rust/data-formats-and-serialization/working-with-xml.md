---
title:                "XML এর সাথে কাজ করা"
date:                  2024-03-17T18:36:15.868854-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

XML, যার পূর্ণ নাম এক্সটেন্সিবল মার্কআপ ল্যাঙ্গুয়েজ, JSON-এর বিস্তারিত আত্মীয় হিসাবে চিহ্নিত। আপনি যখন পুরাতন সিস্টেমস, এন্টারপ্রাইজ সফটওয়্যার, অথবা এমন APIs-এর সাথে কাজ করবেন যা JSON ব্যান্ড্যাগনে অংশ নেয়নি, তখন XML-এর সাথে আপনার লড়াই হবে। যেখানে XML তার মাটি ধরে রাখে, সেখানে ডাটা এক্সচেঞ্জের জন্য এটি অপরিহার্য।

## কিভাবে:

Rust-এ, আপনি `xml-rs` এর মতো crates ব্যবহার করে XML হ্যান্ডল করতে পারেন। `Cargo.toml`-এ `xml-rs = "0.8"` যুক্ত করে ইনস্টল করুন। এখানে একটি সাধারণ XML পার্স করার পদ্ধতি তুলে ধরা হলো:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("শুরু: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("টেক্সট: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("শেষ: {}", name);
            }
            Err(e) => {
                println!("ত্রুটি: {}", e);
            }
            _ => {}
        }
    }
}
```

আউটপুট:
```
শুরু: book
শুরু: title
টেক্সট: Rust in Action
শেষ: title
শুরু: author
টেক্সট: Tim McNamara
শেষ: author
শুরু: year
টেক্সট: 2021
শেষ: year
শেষ: book
```
এই কোড XML স্ট্রিম-রিড করে, শুরু এবং শেষ এলিমেন্টস এবং টেক্সট ডাটা হ্যান্ডল করে, প্রতিটি ধাপের তথ্য লগিং করে।

## গভীর ডাইভ:

প্রযুক্তির বয়সে XML একজন সিনিয়র, ৯০-এর দশকের শেষে ওয়েবের জন্য তৈরি। এর ডিজাইন পঠনযোগ্যতা (মেশিন এবং মানুষের উভয়ের জন্য) এবং বিস্তারিত স্ব-বর্ণনামূলক ডাটা উত্তেজনা সঞ্চার করে।

বিকল্প? নিশ্চয়ই, JSON ওয়েব APIs-এর জন্য আধুনিক গন্তব্য, হালকা এবং কম সমৃদ্ধ। এদিকে, YAML এর পরিষ্কার লেআউটের জন্য configs-এর জন্য ভক্ত কুঁড়িয়েছে। কিন্তু XML শীঘ্রই কোথাও যাচ্ছে না—বিশাল অবকাঠামো এর পিঠে নির্মিত।

গোপনে, Rust-এর XML পার্সিং ইটারেটর প্যাটার্নসের উপর ভর করে, মেমরি ব্যবহার কম রেখে এবং পারফরম্যান্স তীক্ষ্ন রাখে। আপনি `serde-xml-rs` এর মতো crates খুঁজে পাবেন JSON হ্যান্ডলিং-এ অভ্যস্তদের জন্য আরও সের্দে-লাইক অভিজ্ঞতার জন্য—একটি বর্ধন।

## আরও দেখুন:
Rust এবং XML সম্পর্কে আরও জানতে: 
- Rust-এর সের্দে সামঞ্জস্যের জন্য `serde-xml-rs`: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- অফিসিয়াল Rust ডকুমেন্টেশন (কারণ মাঝে মাঝে ব্রাশ আপ করা ক্ষতির নয়): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
