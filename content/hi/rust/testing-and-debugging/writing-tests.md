---
title:                "टेस्ट लिखना"
aliases:
- /hi/rust/writing-tests/
date:                  2024-02-03T19:32:59.887448-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Rust में टेस्ट लिखना आपके कोड के अपेक्षित प्रदर्शन को सुनिश्चित करने के लिए स्वचालित जाँच बनाना शामिल है। प्रोग्रामर्स यह जल्दी बग्स पकड़ने, रिफैक्टरिंग को सहायता करने, और समय के साथ कोड की गुणवत्ता को बनाए रखने के लिए करते हैं।

## कैसे करें:

Rust का निर्मित परीक्षण ढांचा बाहरी लाइब्रेरियों की आवश्यकता के बिना यूनिट, इंटीग्रेशन, और दस्तावेज़ीकरण परीक्षणों को समर्थन करता है। परीक्षण `#[test]` के साथ एनोटेट किए जाते हैं, और इस तरह से एनोटेट किया गया कोई भी फ़ंक्शन एक परीक्षण के रूप में संकलित होता है।

### एक यूनिट टेस्ट लिखना:

यूनिट टेस्ट को `#[cfg(test)]` के साथ चिह्नित `tests` उप-मॉड्यूल का उपयोग करके उस मॉड्यूल में रखें जिसे वे परीक्षण कर रहे हैं ताकि सुनिश्चित किया जा सके कि वे केवल परीक्षण के समय संकलित हों।

```rust
// lib.rs या main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

परीक्षण चलाना:
```shell
$ cargo test
```

आउटपुट:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (या src/main.rs)

1 परीक्षण चल रहा है
test tests::it_adds_two ... ok

परीक्षण परिणाम: ok. 1 पास; 0 विफल; 0 अनदेखी; 0 मापा गया; 0 बाहर छलनी किया गया
```

### इंटीग्रेशन टेस्ट लिखना:

इंटीग्रेशन टेस्ट आपकी परियोजना के शीर्ष स्तर पर `src` के बगल में एक tests निर्देशिका में जाते हैं। `tests` में प्रत्येक `.rs` फ़ाइल को अपने स्वयं के अलग क्रेट के रूप में संकलित किया जाता है।

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### लोकप्रिय तृतीय-पक्ष लाइब्रेरीज के साथ परीक्षण:

अधिक व्यापक परीक्षण क्षमताओं के लिए, `proptest` लाइब्रेरी परीक्षण फ़ंक्शनों को परीक्षण करने के लिए व्यापक रेंज के इनपुट्स उत्पन्न कर सकती है।

`Cargo.toml` में `proptest` को एक dev dependency के रूप में जोड़ें:

```toml
[dev-dependencies]
proptest = "1.0"
```

कई स्वचालित रूप से उत्पन्न इनपुट्स के साथ एक ही परीक्षण चलाने के लिए `proptest` का उपयोग करें:

```rust
// tests/integration_test.rs में या एक मॉड्यूल के #[cfg(test)] में

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

यह जांचता है कि `add` एक व्यापक रेंज के `i32` इनपुट्स के लिए पैनिक नहीं करता है।
