---
date: 2024-01-20 17:43:10.776672-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u0930\u0938\
  \u094D\u091F \u092E\u0947\u0902 \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947\
  \ \u092E\u093F\u0932\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\
  \u094B \u0939\u091F\u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0906\u092A\
  \ `replace` \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u093E \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\
  \u0948\u0902\u0964."
lastmod: '2024-04-05T22:38:52.839431-06:00'
model: gpt-4-1106-preview
summary: ") \u0930\u0938\u094D\u091F \u092E\u0947\u0902 \u092A\u0948\u091F\u0930\u094D\
  \u0928 \u0938\u0947 \u092E\u093F\u0932\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\
  \u094B\u0902 \u0915\u094B \u0939\u091F\u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u0906\u092A `replace` \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902\u0964."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## कैसे करें? (How to:)
रस्ट में पैटर्न से मिलते अक्षरों को हटाने के लिए आप `replace` फ़ंक्शन का इस्तेमाल कर सकते हैं।

```Rust
fn main() {
    let original_string = "Hello, 123 world! 456";
    let pattern = "[0-9]";
    let replaced_string = regex::Regex::new(pattern).unwrap().replace_all(&original_string, "");
    println!("Modified String: {}", replaced_string);
}
```

इस कोड का आउटपुट होगा:
```
Modified String: Hello,  world!
```

## गहराई से समझिए (Deep Dive)
पैटर्न को रिप्लेस करने की अवधारणा टेक्स्ट प्रोसेसिंग में बहुत पुरानी है और रेग्युलर एक्सप्रेशन्स (regular expressions) का इतिहास 1950 के दशक में शुरू हुआ था। रस्ट में पैटर्न के साथ काम करने के लिए `regex` क्रेट का इस्तेमाल होता है। यह जटिल पैटर्न मैचिंग को संभव बनाता है और इसका पर्फॉरमेंस भी काफी अच्छा है। इसके विकल्प के रूप में, साधारण स्ट्रिंग रिप्लेसमेंट फंक्शंस भी होते हैं जैसे `replace`, जो कि पैटर्न मैचिंग के बिना सीधे स्ट्रिंग्स बदल देते हैं।

## यह भी देखें (See Also)
- रस्ट की आधिकारिक डॉक्यूमेंटेशन के रेग्युलर एक्सप्रेशंस: https://doc.rust-lang.org/regex/regex/index.html
- `regex` क्रेट स्रोतह: https://crates.io/crates/regex
- रस्ट बाय एक्साम्पल पर स्ट्रिंग्स की एक गाइड: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
