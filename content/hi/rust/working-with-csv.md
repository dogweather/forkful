---
title:                "CSV के साथ काम करना"
date:                  2024-02-03T19:22:37.425892-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV (Comma-Separated Values) फ़ाइलों के साथ काम करना मतलब तालिकात्मक डाटा संग्रहित करने वाली सादा पाठ फाइलों से पढ़ना और उनमें लिखना है। प्रोग्रामर ऐसा विभिन्न प्रोग्रामों, सिस्टमों के बीच डाटा साझा करने या बड़े डाटा सेटों को एक कुशल, मानव-पठनीय प्रारूप में संसाधित करने के लिए करते हैं।

## कैसे करें:
रस्ट, अपने सुरक्षा और प्रदर्शन पर ध्यान केंद्रित करने के साथ, CSV फ़ाइलों के साथ डील करने के लिए उत्कृष्ट क्रेट्स (लाइब्रेरीज़) प्रदान करता है, `csv` सबसे लोकप्रिय होने के साथ। आपको डाटा को सीरियलाइज़ और डीसीरियलाइज़ करने के लिए `serde` की भी आवश्यकता होगी।

पहले, अपने `Cargo.toml` में निर्भरताएं जोड़ें:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### CSV पढ़ना

किसी CSV फ़ाइल को पढ़ने के लिए, एक स्ट्रक्चर परिभाषित करें जो आपके डाटा का प्रतिनिधित्व करता है और `serde` से `Deserialize` को व्युत्पन्न करें:

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
        println!("उदाहरण चलाने में त्रुटि: {}", err);
        process::exit(1);
    }
}
```

किसी CSV के लिए नमूना आउटपुट जिसमें शहर की जानकारी हो सकती है:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### CSV में लिखना

किसी CSV फ़ाइल में लिखने के लिए, एक स्ट्रक्चर परिभाषित करें और `Serialize` को व्युत्पन्न करें:

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

इससे `output.csv` फ़ाइल डाटा के साथ बनेगी:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

रस्ट की शक्तिशाली प्रकार प्रणाली और इकोसिस्टम के मजबूत क्रेट्स का लाभ उठाकर, CSV डाटा के साथ काम करना दोनों ही कुशल और सरल हो जाता है, अपने डाटा प्रोसेसिंग कार्यों में सुरक्षा और प्रदर्शन सुनिश्चित करता है।
