---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:46.453441-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  \u09C7\u09B0 \u09AE\u09BE\u09A8\u09A6\u09A3\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 YAML \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\
  \u099F \u0995\u09B0\u09C7 \u09A8\u09BE, \u09A4\u09BE\u0987 \u0986\u09AE\u09B0\u09BE\
  \ \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 `serde` (\u09A1\u09C7\u099F\u09BE \u09B8\
  \u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C \u0993 \u09A1\u09BF\
  \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF) \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 `serde_yaml`\u2026"
lastmod: '2024-03-17T18:47:43.831286-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F\u09C7\u09B0 \u09AE\u09BE\u09A8\u09A6\u09A3\
  \u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7\
  \ YAML \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u0995\u09B0\u09C7 \u09A8\u09BE\
  , \u09A4\u09BE\u0987 \u0986\u09AE\u09B0\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09A4 `serde` (\u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u09B2\u09BE\u0987\u099C \u0993 \u09A1\u09BF\u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\
  \u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  ) \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 `serde_yaml` \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09BF\u0964\n\n\u09AA\u09CD\u09B0\
  \u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 `Cargo.toml`-\u098F \u09A8\u09BF\
  \u09B0\u09CD\u09AD\u09B0\u09A4\u09BE \u09AF\u09CB\u0997 \u0995\u09B0\u09C1\u09A8\
  ."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
রাস্টের মানদণ্ড লাইব্রেরিতে YAML সাপোর্ট করে না, তাই আমরা সাধারণত `serde` (ডেটা সিরিয়ালাইজ ও ডিসিরিয়ালাইজ করার জন্য) এর সাথে `serde_yaml` ব্যবহার করে থাকি।

প্রথমে, আপনার `Cargo.toml`-এ নির্ভরতা যোগ করুন:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

এখন, দেখা যাক কিভাবে একটি YAML স্ট্রিংকে রাস্ট স্ট্রাকচারে ডিসিরিয়ালাইজ করতে হয় এবং একটি রাস্ট স্ট্রাকচারকে আবার YAML স্ট্রিংয়ে সিরিয়ালাইজ করতে হয়।

### YAML কে রাস্ট স্ট্রাকচারে ডিসিরিয়ালাইজ করা
এমন একটি রাস্ট স্ট্রাকচার ডিফাইন করুন যা YAML-এ আপনার প্রত্যাশিত ডেটা অনুসরণ করে। প্রয়োজনে কাস্টমাইজেশনের জন্য Serde এট্রিবিউট ব্যবহার করুন।

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

উপরের রাস্ট কোড চালানোর ফলে স্যাম্পল আউটপুট হবে:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### রাস্ট স্ট্রাকচার কে YAML এ সিরিয়ালাইজ করা
এই উদাহরণটি পূর্ববর্তী অংশ থেকে `Config` স্ট্রাকচারটি নেয় এবং এটিকে আবার YAML ফরম্যাটে সিরিয়ালাইজ করে।

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

প্রত্যাশিত আউটপুট হবে একটি YAML-ফর্ম্যাটেড স্ট্রিং:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

এই স্নিপেটগুলি দেখায় কিভাবে আপনার রাস্ট অ্যাপ্লিকেশনে জনপ্রিয় `serde` এবং `serde_yaml` ক্রেটগুলি ব্যবহার করে YAML পার্সিং ও জেনারেশনকে কার্যকরীভাবে ইন্টিগ্রেট করা যায়, জটিল ডেটা স্ট্রাকচার মোকাবিলা করে ও সাধারণ, মানব-পাঠ্য কনফিগারেশন প্রদান করে।
