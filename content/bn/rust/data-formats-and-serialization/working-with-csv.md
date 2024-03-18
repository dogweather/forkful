---
title:                "CSV এর সাথে কাজ করা"
date:                  2024-03-17T18:28:41.355422-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
CSV (কমা-সেপারেটেড ভ্যালুস) ফাইল নিয়ে কাজ করা মানে হল টেবিলার ডাটা সংরক্ষণ করে এমন প্লেইন টেক্সট ফাইল থেকে পড়া এবং লিখা। প্রোগ্রামাররা বিভিন্ন প্রোগ্রাম, সিস্টেমের মধ্যে ডাটা শেয়ারিং বা বড় ডাটা সেটের কাজ দক্ষতার সাথে মানব-পঠনযোগ্য ফরম্যাটে করার জন্য এটি করে থাকেন।

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
