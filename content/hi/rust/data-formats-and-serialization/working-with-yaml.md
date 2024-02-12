---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:27:36.081364-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Rust प्रोग्रामिंग में, YAML (YAML Ain't Markup Language) के साथ काम करना YAML प्रारूप में डेटा पार्सिंग और जनरेटिंग के बारे में है, जो एक मानव-अनुकूल डेटा सीरियलाइजेशन मानक है। प्रोग्रामर एप्लीकेशन को कॉन्फ़िगर करने, सेटिंग्स प्रबंधन, या जटिल डेटा संरचनाओं को एक स्पष्ट और पठनीय प्रारूप में प्रोसेस करने के लिए, इसकी सादगी का लाभ उठाते हुए, कॉन्फ़िगरेशन फ़ाइलों और डेटा आदान-प्रदान के लिए JSON या XML पर Rust में YAML हैंडलिंग इंटीग्रेट करते हैं।

## कैसे करें:

Rust अपनी स्टैंडर्ड लाइब्रेरी में YAML का समर्थन नहीं करता, इसलिए हम आमतौर पर `serde` (डेटा को सीरियलाइज और डीसीरियलाइज करने के लिए) जैसे तृतीय-पक्ष क्रेट्स का उपयोग `serde_yaml` के संयोजन में करते हैं।

पहले, अपनी `Cargo.toml` में निर्भरताएँ जोड़ें:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

अब, देखते हैं कि कैसे एक YAML स्ट्रिंग को एक Rust struct में डीसीरियलाइज करें और एक Rust struct को वापस एक YAML स्ट्रिंग में सीरियलाइज करें।

### YAML को Rust संरचनाओं में डीसीरियलाइज करना

उस डेटा को दर्पणित करते हुए एक Rust struct परिभाषित करें जिसे आप YAML में उम्मीद करते हैं। यदि आवश्यक हो, तो कस्टमाइज़ेशन के लिए Serde विशेषताओं का उपयोग करें।

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

उपरोक्त Rust कोड चलाने पर नमूना आउटपुट होगा:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Rust संरचनाओं को YAML में सीरियलाइज़ करना

यह उदाहरण पिछले अनुभाग से `Config` struct को लेता है और इसे वापस YAML प्रारूप में सीरियलाइज़ करता है।

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

अपेक्षित आउटपुट एक YAML-प्रारूपित स्ट्रिंग होगा:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

ये स्निपेट्स आपके Rust एप्लीकेशन्स में YAML पार्सिंग और जनरेशन को कुशलतापूर्वक इंटीग्रेट करने का तरीका दिखाते हैं, लोकप्रिय `serde` और `serde_yaml` क्रेट्स का उपयोग करते हुए, जटिल डेटा संरचनाओं को समायोजित करते हैं और सरल, मानव-पठनीय कॉन्फ़िगरेशन प्रदान करते हैं।
