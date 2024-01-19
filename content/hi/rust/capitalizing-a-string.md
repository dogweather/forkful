---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Rust: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग को कैपिटलाइज़ करना इसके पहले अक्षर को बड़ा करना होता है। प्रोग्रामर इसे यूज़र्स को तब प्रस्तुत करने के लिए करते हैं जब वे एक विशेष संदर्भ को हाइलाइट करना चाहते हैं।

## कैसे करें:

```Rust
fn main() {
    let my_str = "hello, rust!";
    let capitalized_str = my_str.chars()
                                .enumerate()
                                .map(|(i,c)| if i==0 { c.to_uppercase().collect() } else { c.to_lowercase().collect() })
                                .collect::<String>();
    println!("{}", capitalized_str);                                
}
```
ऊपर दिए गए कोड का आउटपुट होगा:

```Rust
"Hello, rust!"
```
## गहराई में: 

हिस्टोरीचल कॉन्टेक्स्ट: यह कन्वेंशन टाइपराइटर से आया है, जहां पराग्राफ की शुरुआत या महत्वपूर्ण श्रृंखलाओं के लिए पहला अक्षर कैपिटलाइज़ होता था। 

वैकल्पिक: Rust में कई तरीके हैं जिससे हम स्ट्रिंग को कैपिटलाइज़ कर सकते हैं। `to_uppercase()` और `enumerate()` का उपयोग करने के अलावा, हम `char_indices()` फंक्शन का उपयोग भी कर सकते हैं। 

खुदराविस्तारी: `to_uppercase()` और `to_lowercase()` फंक्शन Unicode के लिए नियमों को अनुसरण करते हैं, जो कि साधारणतया ASCII से अधिक विशेषज्ञता प्रदान करते हैं।

## और भी देखें:

1. [Rust स्ट्रिंग और उनकі Performance](https://cheats.rs/#strings)
2. [Rust Documentation: Strings](https://doc.rust-lang.org/stable/rust-by-example/primitives.html)