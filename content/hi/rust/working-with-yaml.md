---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"

category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
YAML एक डेटा सीरियलाइजेशन फॉर्मेट है जो डेटा को मानव-पठनीय ढंग से प्रस्तुत करता है। प्रोग्रामर्स इसका उपयोग कॉन्फ़िग फाइल्स, डेटा को स्टोर और ट्रांसफर करने के लिए करते हैं क्योंकि यह इंटरऑपेरेबल और आसानी से समझ में आता है।

## कैसे करें? (How to:)
Rust में YAML से काम करने के लिए `serde_yaml` क्रेट का उपयोग करेंगे। पहले, `Cargo.toml` में डिपेंडेंसी जोड़ें:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8.23"
```

एक सिंपल YAML को पार्स करने का उदाहरण:

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: u64,
    activate: bool,
}

fn main() -> Result<(), serde_yaml::Error> {
    let data = "
name: Excalibur
durability: 1000
activate: true
    ";

    let config: Config = serde_yaml::from_str(data)?;
    println!("{:?}", config);

    Ok(())
}
```

आउटपुट होगा:

```
Config { name: "Excalibur", durability: 1000, activate: true }
```

## गहन जानकारी (Deep Dive)
YAML ("YAML Ain't Markup Language" के लिए) 2001 में विकसित हुआ था। XML और JSON इसके विकल्प हैं, पर YAML मानव-पठनीयता और कॉन्फ़िग फाइल्स के लिए बेहद लोकप्रिय है। Rust में, YAML पार्सिंग मेमोरी सेफ्टी और टाइप सेफ्टी के साथ संभव है, जो `serde` फ्रेमवर्क द्वारा लागू किया गया अध्यापन को इस्तेमाल करता है। 

## संबंधित स्रोत (See Also)
यमल सीखने केलिए और सूत्र:

- YAML ऑफिशियल साइट: [https://yaml.org](https://yaml.org)
- Serde क्रेट की डॉक्युमेंटेशन: [https://docs.rs/serde](https://docs.rs/serde)
- Serde_yaml क्रेट की डॉक्युमेंटेशन: [https://docs.rs/serde_yaml](https://docs.rs/serde_yaml)
- रस्ट प्रोग्रामिंग भाषा की बुक: [https://doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
