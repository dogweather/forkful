---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:46.453441-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

রাস্ট প্রোগ্রামিংয়ে, YAML (YAML Ain't Markup Language) এর সাথে কাজ করা মানে হল ডেটা পার্সিং ও তৈরি করা YAML ফরম্যাটে, যা একটি মানব-বান্ধব ডেটা সিরিয়ালাইজেশন স্ট্যান্ডার্ড। প্রোগ্রামাররা অ্যাপ্লিকেশন কনফিগার করা, সেটিংস ম্যানেজ করা, অথবা জটিল ডেটা স্ট্রাকচার প্রক্রিয়া করা এর জন্য রাস্টে YAML হ্যান্ডলিং ইন্টিগ্রেট করে, কনফিগারেশন ফাইল ও ডেটা এক্সচেঞ্জের জন্য JSON বা XML এর তুলনায় এর সারল্যকে কাজে লাগিয়ে।

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
