---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:41.355422-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BE\u09B8\u09CD\u099F\
  , \u098F\u09B0 \u09A8\u09BF\u09B0\u09BE\u09AA\u09A4\u09CD\u09A4\u09BE \u098F\u09AC\
  \u0982 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09BE\u09B0\u09BF\u09A4\u09BE \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u09AE\u09A8\u09CB\u09A8\u09BF\u09AC\u09C7\u09B6 \u0995\
  \u09B0\u09C7, CSV \u09AB\u09BE\u0987\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u099A\u09AE\u09CE\
  \u0995\u09BE\u09B0 \u0995\u09CD\u09B0\u09C7\u099F\u09B8 (\u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u09B8) \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7, `csv` \u09B9\u09B2 \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7\u2026"
lastmod: '2024-04-05T21:53:52.023844-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BE\u09B8\u09CD\u099F, \u098F\u09B0 \u09A8\u09BF\u09B0\u09BE\u09AA\
  \u09A4\u09CD\u09A4\u09BE \u098F\u09AC\u0982 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\
  \u09BE\u09B0\u09BF\u09A4\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u09AE\u09A8\u09CB\
  \u09A8\u09BF\u09AC\u09C7\u09B6 \u0995\u09B0\u09C7, CSV \u09AB\u09BE\u0987\u09B2\
  \ \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u099A\u09AE\u09CE\u0995\u09BE\u09B0 \u0995\u09CD\u09B0\u09C7\
  \u099F\u09B8 (\u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B8) \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, `csv` \u09B9\u09B2 \u09B8\u09AC\
  \u099A\u09C7\u09AF\u09BC\u09C7 \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC\u0964\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 `serde` \u098F\u09B0\u0993 \u09AA\u09CD\u09B0\u09AF\
  \u09BC\u09CB\u099C\u09A8 \u09B9\u09AC\u09C7 \u09A1\u09BE\u099F\u09BE \u09B8\u09BF\
  \u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C \u098F\u09AC\u0982 \u09A1\
  \u09BF\u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7\
  , \u0986\u09AA\u09A8\u09BE\u09B0 `Cargo.toml` \u09AB\u09BE\u0987\u09B2\u09C7 \u09A8\
  \u09BF\u09B0\u09CD\u09AD\u09B0\u09A4\u09BE\u09B8\u09AE\u09C2\u09B9 \u09AF\u09CB\u0997\
  \ \u0995\u09B0\u09C1\u09A8."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
রাস্ট, এর নিরাপত্তা এবং কার্যকারিতা নিয়ে মনোনিবেশ করে, CSV ফাইল নিয়ে কাজ করার জন্য চমৎকার ক্রেটস (লাইব্রেরিস) প্রদান করে, `csv` হল সবচেয়ে জনপ্রিয়। আপনার `serde` এরও প্রয়োজন হবে ডাটা সিরিয়ালাইজ এবং ডিসিরিয়ালাইজ করার জন্য।

প্রথমে, আপনার `Cargo.toml` ফাইলে নির্ভরতাসমূহ যোগ করুন:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### CSV পড়া
একটি CSV ফাইল পড়ার জন্য, আপনার ডাটাকে প্রতিনিধিত্ব করে এমন একটি কাঠামো (struct) সংজ্ঞায়িত করুন এবং `serde` থেকে `Deserialize` প্রাপ্ত করুন:

```rust
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io;
use std::process;

#[derive(Debug, Deserialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn read_from_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = read_from_csv("cities.csv") {
        println!("উদাহরণ চালানো সমস্যা হয়েছে: {}", err);
        process::exit(1);
    }
}
```

শহরের তথ্য সম্বলিত একটি CSV ফাইলের জন্য নমুনা আউটপুট হিসেবে দেখাতে পারে:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### CSV তে লিখুন
CSV ফাইলে লেখার জন্য, একটি কাঠামো সংজ্ঞায়িত করুন এবং `Serialize` প্রাপ্ত করুন:

```rust
use serde::Serialize;
use std::error::Error;
use std::fs::File;

#[derive(Serialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn write_to_csv(file_path: &str, records: Vec<Record>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = csv::Writer::from_writer(file);

    for record in records {
        wtr.serialize(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let records = vec![
        Record {
            city: "Los Angeles".into(),
            state: "CA".into(),
            population: 3979563,
        },
        Record {
            city: "Chicago".into(),
            state: "IL".into(),
            population: 2695598,
        },
    ];

    write_to_csv("output.csv", records)?;

    Ok(())
}
```

এটি `output.csv` ফাইল তৈরি করবে যাতে ডাটা থাকবে:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

রাস্টের শক্তিশালী টাইপ সিস্টেম এবং একোসিস্টেমের শক্তিশালী ক্রেটসের সাহায্যে, CSV ডাটা নিয়ে কাজ করা দক্ষ এবং সরল হয়ে উঠে, আপনার ডাটা প্রক্রিয়াকরণ কাজে নিরাপত্তা এবং কার্যকারিতা নিশ্চিত করে।
