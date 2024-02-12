---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
aliases:
- /hi/rust/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:43:10.776672-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
पैटर्न से मिलते अक्षरों को हटाना मतलब है कुछ खास नियमों के आधार पर स्ट्रिंग से चुनिंदा अक्षरों को दूर करना। प्रोग्रामर्स इसे डेटा से अनावश्यक या खतरनाक तत्वों को निकालने, या इसे और अधिक पठनीय बनाने के लिए करते हैं।

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
