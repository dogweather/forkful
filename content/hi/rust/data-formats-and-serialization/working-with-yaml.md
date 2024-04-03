---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:36.081364-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Rust \u0905\u092A\
  \u0928\u0940 \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092E\u0947\u0902 YAML \u0915\u093E\
  \ \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\u0939\u0940\u0902 \u0915\u0930\u0924\
  \u093E, \u0907\u0938\u0932\u093F\u090F \u0939\u092E \u0906\u092E\u0924\u094C\u0930\
  \ \u092A\u0930 `serde` (\u0921\u0947\u091F\u093E \u0915\u094B \u0938\u0940\u0930\
  \u093F\u092F\u0932\u093E\u0907\u091C \u0914\u0930 \u0921\u0940\u0938\u0940\u0930\
  \u093F\u092F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F) \u091C\u0948\u0938\u0947\u2026"
lastmod: '2024-03-13T22:44:51.998290-06:00'
model: gpt-4-0125-preview
summary: "Rust \u0905\u092A\u0928\u0940 \u0938\u094D\u091F\u0948\u0902\u0921\u0930\
  \u094D\u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092E\u0947\
  \u0902 YAML \u0915\u093E \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\u0939\u0940\
  \u0902 \u0915\u0930\u0924\u093E, \u0907\u0938\u0932\u093F\u090F \u0939\u092E \u0906\
  \u092E\u0924\u094C\u0930 \u092A\u0930 `serde` (\u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C \u0914\u0930 \u0921\u0940\
  \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F) \u091C\u0948\u0938\u0947 \u0924\u0943\u0924\u0940\
  \u092F-\u092A\u0915\u094D\u0937 \u0915\u094D\u0930\u0947\u091F\u094D\u0938 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 `serde_yaml` \u0915\u0947 \u0938\u0902\u092F\
  \u094B\u091C\u0928 \u092E\u0947\u0902 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  \u0964\n\n\u092A\u0939\u0932\u0947, \u0905\u092A\u0928\u0940 `Cargo.toml` \u092E\
  \u0947\u0902 \u0928\u093F\u0930\u094D\u092D\u0930\u0924\u093E\u090F\u0901 \u091C\
  \u094B\u0921\u093C\u0947\u0902."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

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
