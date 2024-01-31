---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:17:05.593173-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वर्तमान तारीख निकालना मतलब आज की तारीख पता करना है। प्रोग्रामर्स लॉगिंग, रिपोर्टिंग, और इंटरफेस में तिथि दिखाने के लिए यह करते हैं।

## How to: (कैसे करें:)
Rust में `chrono` क्रेट का उपयोग करके आप आसानी से वर्तमान तारीख निकाल सकते हैं। `Cargo.toml` में `chrono` क्रेट जोड़ें। फिर कोड लिखें:

```rust
use chrono::{Local, Datelike};

fn main() {
    let current_date = Local::today();
    println!("आज की तारीख: {}", current_date);
}
```

उदाहरण आउटपुट हो सकता है:

```
आज की तारीख: 2023-03-30
```

## Deep Dive (गहराई में जानकारी)
`chrono` क्रेट Rust भाषा में समय और तारीख संबंधित कार्यों के लिए एक लोकप्रिय लाइब्रेरी है। यह क्रेट Rust के संस्करण 1.0 से पहले आया था और Rust के समय प्रबंधन इकोसिस्टम में मानक के रूप में स्थिर हुआ है। इसमें विभिन्न कैलेंडर सिस्टम्स और टाइम ज़ोन को संभालने की क्षमता है। 

विकल्पों की बात करें तो, Rust में स्टैंडर्ड लाइब्रेरी भी `SystemTime` क्लास प्रदान करती है, जिसे आप बिना किसी एक्सटर्नल क्रेट के उपयोग कर सकते हैं। लेकिन, `chrono` पैकेज अधिक सुविधाजनक और शक्तिशाली है क्योंकि यह अधिक जटिल समय कार्यों जैसे कि टाइम ज़ोन्स और डेट फॉर्मेटिंग का समर्थन करता है।

## See Also (और जानें)
- [chrono crate documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [The Rust Programming Language – Time](https://doc.rust-lang.org/book/ch20-01-single-threaded.html#time)
