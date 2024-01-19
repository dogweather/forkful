---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग से तारीख पार्स करना एक क्रिया होती है जिसमें हम एक स्ट्रिंग को समझकर तारीख में बदल देते हैं। प्रोग्रामर इसे तारीख से संबंधित डाटा संभालने और मानिपुलेट करने के लिए करते हैं।

## कैसे करें:

यहां रस्ट प्रोग्रामिंग भाषा में देखते हैं कि कैसे एक स्ट्रिंग को "2020-04-12" के रूप में एक दिनांक में परिवर्तित किया जा सकता है।

```rust
use chrono::{NaiveDate, ParseError};

fn parse_date_string(date_string: &str) -> Result<NaiveDate, ParseError> {
     NaiveDate::parse_from_str(date_string, "%Y-%m-%d")
}

fn main() {
     let date_string = "2020-04-12";
     match parse_date_string(date_string) {
         Ok(date) => println!("Date parsed: {}", date),
         Err(e) => println!("Failed to parse date: {}", e),
     }
}
```

## गहरा डाइव:

- **ऐतिहासिक संदर्भ**: पहले, डेट पार्सिंग के लिए डेवलपर्स को खुद की प्राथमिकताओं के अनुसार एक कस्टम फ़ंक्शन लिखना होता था। लेकिन चरमा की मदद से, यहां हमें एक तारीख को पार्स करने के लिए एक साधारण और प्रभावी तरीका प्रदान किया गया है।

- **विकल्पों**: अन्य भाषाओं जैसे कि JavaScript, Python, और अन्यों में भी डेट पार्सिंग के लिए विशिष्ट फ़ंक्शन्स मौजूद हैं। हो सकता है कि आपका काम इन भाषाओं में से किसी के साथ हो, और यदि ऐसा है, तो आप उनकी डॉक्यूमेंटेशन को देख सकते हैं। 

- **कार्यान्वयन विवरण**: `NaiveDate::parse_from_str` फ़ंक्शन एक तारीख के स्ट्रिंग प्रतिनिधित्व को पार्स करके `NaiveDate` रिटर्न करता है। यह फ़ंक्शन आपके द्वारा निर्दिष्ट फ़ॉर्मेट का अनुसरण करता है। 

## देखें भी:

- Rust प्रोग्रामिंग भाषा (https://www.rust-lang.org)
- Chrono, a Rust date/time library (https://docs.rs/chrono/0.4.19/chrono/)
- Date/time formatting syntax (https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html)