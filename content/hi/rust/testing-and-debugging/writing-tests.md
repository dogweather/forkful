---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:59.887448-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Rust \u0915\u093E\
  \ \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u092A\u0930\u0940\u0915\u094D\u0937\
  \u0923 \u0922\u093E\u0902\u091A\u093E \u092C\u093E\u0939\u0930\u0940 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u093F\u092F\u094B\u0902 \u0915\u0940 \u0906\
  \u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0915\u0947 \u092C\u093F\u0928\u093E\
  \ \u092F\u0942\u0928\u093F\u091F, \u0907\u0902\u091F\u0940\u0917\u094D\u0930\u0947\
  \u0936\u0928, \u0914\u0930 \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\u093C\
  \u0940\u0915\u0930\u0923 \u092A\u0930\u0940\u0915\u094D\u0937\u0923\u094B\u0902\
  \ \u0915\u094B \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u092A\u0930\u0940\u0915\u094D\u0937\u0923\u2026"
lastmod: '2024-04-05T21:53:53.968304-06:00'
model: gpt-4-0125-preview
summary: "Rust \u0915\u093E \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u092A\u0930\
  \u0940\u0915\u094D\u0937\u0923 \u0922\u093E\u0902\u091A\u093E \u092C\u093E\u0939\
  \u0930\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u093F\u092F\u094B\u0902\
  \ \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0915\u0947 \u092C\
  \u093F\u0928\u093E \u092F\u0942\u0928\u093F\u091F, \u0907\u0902\u091F\u0940\u0917\
  \u094D\u0930\u0947\u0936\u0928, \u0914\u0930 \u0926\u0938\u094D\u0924\u093E\u0935\
  \u0947\u091C\u093C\u0940\u0915\u0930\u0923 \u092A\u0930\u0940\u0915\u094D\u0937\u0923\
  \u094B\u0902 \u0915\u094B \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 `#[test]` \u0915\
  \u0947 \u0938\u093E\u0925 \u090F\u0928\u094B\u091F\u0947\u091F \u0915\u093F\u090F\
  \ \u091C\u093E\u0924\u0947 \u0939\u0948\u0902, \u0914\u0930 \u0907\u0938 \u0924\u0930\
  \u0939 \u0938\u0947 \u090F\u0928\u094B\u091F\u0947\u091F \u0915\u093F\u092F\u093E\
  \ \u0917\u092F\u093E \u0915\u094B\u0908 \u092D\u0940 \u092B\u093C\u0902\u0915\u094D\
  \u0936\u0928 \u090F\u0915 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0915\u0947\
  \ \u0930\u0942\u092A \u092E\u0947\u0902 \u0938\u0902\u0915\u0932\u093F\u0924 \u0939\
  \u094B\u0924\u093E \u0939\u0948\u0964."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

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
