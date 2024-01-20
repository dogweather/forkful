---
title:                "CSV के साथ काम करना"
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV, यानी Comma-Separated Values, एक सरल फॉर्मेट है जो डेटा को टेबल फॉर्म में स्टोर करता है। प्रोग्रामर्स इसका उपयोग डेटा को आसानी से इम्पोर्ट और एक्सपोर्ट करने के लिए करते हैं, खासकर जब डेटाबेस और स्प्रेडशीट्स के साथ काम करते हैं।

## How to: (कैसे करें:)
Rust में CSV पढ़ना और लिखना बहुत सीधा है। यहाँ `csv` क्रेट का उपयोग करके एक साधारण उदाहरण दिया गया है:

```Rust
use std::error::Error;
use std::fs::File;
use std::process;

use csv::ReaderBuilder;
use csv::Writer;

fn read_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let mut rdr = ReaderBuilder::new().from_path(file_path)?;
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn write_csv(file_path: &str, records: Vec<Vec<String>>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = Writer::from_writer(file);
    
    for record in records {
        wtr.write_record(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() {
    if let Err(err) = read_csv("data.csv") {
        println!("Error reading CSV: {}", err);
        process::exit(1);
    }

    let records = vec![
        vec!["Name".to_string(), "Place".to_string(), "ID".to_string()],
        vec!["Ramesh".to_string(), "Delhi".to_string(), "1".to_string()]
    ];

    if let Err(err) = write_csv("output.csv", records) {
        println!("Error writing CSV: {}", err);
        process::exit(1);
    }
}
```

इस कोड के चलने पर, यह `data.csv` से रिकॉर्ड्स को पढ़ेगा और `output.csv` में नए रिकॉर्ड्स को लिखेगा।

## Deep Dive (गहराई में जानकारी):
CSV हैंडलिंग की जरूरत जब से नुमाया हुई, तब से कई लाइब्रेरीज़ और टूल्स डेवलप हो चुके हैं। Rust में `csv` क्रेट इस काम के लिए सबसे लोकप्रिय है, जिसमें सहज पार्सिंग और बेहतर एरर हैंडलिंग शामिल है। विकल्पों में `serde` का इस्तेमाल करके स्ट्रक्चर्ड डेटा में सीरिअलाइज़ और डीसीरिअलाइज़ करना शामिल है। इम्प्लीमेंटेशन डिटेल्स में, स्ट्रीमिंग रीड/राइट ऑपरेशंस और लार्ज डेटा सेट्स के लिए बफ़रिंग पर ध्यान दिया गया है।

## See Also (इसे भी देखें):
- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [CSV Crate Documentation](https://docs.rs/csv/latest/csv/)
- [Serde Crate Documentation](https://serde.rs/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/)