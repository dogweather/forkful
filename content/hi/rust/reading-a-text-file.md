---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फाइल पढ़ना, यानी कि टेक्स्ट फाइल को पेश करने वाले डेटा को पर्स करना इसका मतलब है। यह प्रोग्रामर्स के लिए महत्वपूर्ण है क्योंकि यह डेटा को बचाने और बाद में पुन: उपयोग करने का एक साधारण तरीका है।

## ऐसे कैसे:

नीचे Rust उदाहरण में दिखाया गया है कि कैसे एक टेक्स्ट फ़ाइल को पढ़ा जा सकता है:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("foo.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("Content:\n{}", contents);

    Ok(())
}
```

यह कोड `foo.txt` नामक फाइल को खोलता है, फिर उसकी सारी सामग्री को एक स्ट्रिंग में पढ़ता है, और अंत में, प्रिंट करता है। 

## गहराई से जानकारी:

टेक्स्ट फ़ाइलों को पढ़ने का इतिहास संगणक विज्ञान के सबसे पुराने दिनों से आ रहा है। Rust जैसी आधुनिक भाषाएँ फ़ाइल पढ़ने के लिए श्रेष्ठ तरीकों पर काम करने और फाइल I/O ऑपरेशन्स को आसान करने का प्रयास करती हैं। और पठन का तरीका फाइल की प्रकृति और आपकी आवश्यकताओं पर आधारित है। अगर आपको एक पूरी फाइल को लोड करने की आवश्यकता है, `read_to_string` एक अच्छा विकल्प है। 

वैकल्पिक टारिके हो सकते हैं, जैसे कि `BufReader` और `lines` method, जो प्रत्येक लाइन को एक बार में अलग से पढ़ते हैं क्या बड़ी फ़ाइलों के साथ उपयोगी हो सकता है।

## और भी देखें:

यदि आप और जानना चाहते हैं, तो यदि आप यहाँ जा सकते हैं:
1. Rust की आधिकारिक पुस्तक: [File I/O](https://doc.rust-lang.org/stable/book/ch12-02-reading-a-file.html)
2. एक अच्छा ब्लॉग पोस्ट: [Rust से फाइल संबंधित I/O करना](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-io-idioms.html).