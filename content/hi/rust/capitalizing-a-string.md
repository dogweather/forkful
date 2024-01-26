---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग कैपिटलाइज़ेशन मतलब है हर शब्द के पहले अक्षर को बड़ा (कैपिटल) बनाना। प्रोग्रामर्स यह इसलिए करते हैं ताकि टेक्स्ट और भी साफ और औपचारिक लगे, जैसे कि खिताब या नाम में।

## How to: (कैसे करें:)
```Rust
fn capitalize_words(s: &str) -> String {
    s.split_whitespace()
        .map(|word| word.chars().enumerate().map(|(i, c)| if i == 0 { c.to_uppercase().to_string() } else { c.to_string() }).collect())
        .collect::<Vec<_>>()
        .join(" ")
}

fn main() {
    let my_sentence = "नमस्ते, यह एक स्ट्रिंग है।";
    println!("{}", capitalize_words(my_sentence));
}

// आउटपुट: "नमस्ते, यह एक स्ट्रिंग है।"
```

## Deep Dive (गहराई में जानकारी):
स्ट्रिंग कैपिटलाइज़ेशन का इस्तेमाल बहुत पहले से हो रहा है, शुरुआती प्रिटिंग प्रेस डिजाइन से लेकर डिजिटल टेक्स्ट प्रोसेसिंग तक। Rust में स्ट्रिंग कैपिटलाइज़ेशन नहीं होती एक सरल फंक्शन के रूप में; हमें मैन्युअली हर शब्द के पहले अक्षर को बड़ा करना पड़ता है जैसा कि `capitalize_words` फंक्शन में दिखाया गया है।

`split_whitespace` का इस्तेमाल हर शब्द को अलग करने के लिए किया जाता है, फिर `map` और `enumerate` का उपयोग कर के हर अक्षर पर चलते हुए पहले अक्षर को बड़ा किया जाता है। अंत में, `join` से सभी शब्दों को वापस एक स्ट्रिंग में जोड़ा जाता है।

विकल्प के तौर पर, कुछ लाइब्रेरीज़ भी मिलती हैं जो स्ट्रिंग कैपिटलाइज़ेशन के लिए अधिक उन्नत टूल्स प्रदान करती हैं।

## See Also (और देखें):
- Rust Documentation: https://doc.rust-lang.org/book/
- The `capitalize` crate on crates.io: https://crates.io/crates/capitalize
- Rust by Example: https://doc.rust-lang.org/rust-by-example/
