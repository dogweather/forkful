---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# "## क्या और क्यों?"

**सबस्ट्रिंग के निकालना** का मतलब होता है किस्लिए एक विशेष अंश को मूल स्ट्रिंग से अलग करना। प्रोग्रामर्स इसे आमतौर पर पाठ संसाधनों को विश्लेषित करने या उनके साथ काम करने के लिए करते हैं।

# "## कैसे करें:"

आइए देखते हैं कांस्टेंट इंडेक्स का उपयोग करके Rust में सबस्ट्रिंग कैसे निकाला जा सकता है:

```Rust
fn main() {
    let s = "Hello, world!";
    let hello = &s[0..5];
    println!("{}", hello);
}
```

इस कोड का आउटपुट होगा: 

`Hello`

# "## Deep Dive":

हम इस तेरीके का उपयोग कर सकते हैं क्योंकि Rust में स्ट्रिंग्स Unicode स्केलर्स के अनुक्रम के रूप में संगणकित होती हैं। वैसे भी, इस दृष्टिकोण में एक चुनौती हो सकती है क्योंकि सभी Unicode स्केलर्स एक समान लंबाई के नहीं होते। 

इसके वैकल्पिक उपयोग के रूप में, आप `.split()` मेथड का उपयोग कर सकते हैं, जो आपको एक इटरेटर प्रदान करता है जो हर स्केलार वाल्यू पर चलता है। 

# "## देखने के लिए भी:"

Rust कोडिंग टिप्स प्राप्त करने के लिए, [यहां](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html) क्लिक करें। Rust Unicode और स्ट्रिंग्स के विवरण के बारे में और जानकारी [यहां](https://www.ameyalokare.com/rust/2017/10/23/rust-string-vs-str.html) प्राप्त करें।