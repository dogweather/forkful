---
date: 2024-01-20 17:57:23.878537-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to) Rust \u092E\
  \u0947\u0902 \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\
  \u094D\u0917\u094D\u092F\u0942\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\
  \u0947\u0902 \u0915\u0941\u091B \u0910\u0938\u0947."
lastmod: '2024-04-05T22:38:52.891903-06:00'
model: gpt-4-1106-preview
summary: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to) Rust \u092E\u0947\
  \u0902 \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u094D\u092F\u0942\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0947\
  \u0902 \u0915\u0941\u091B \u0910\u0938\u0947."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## कैसे करें? (How to)
Rust में कमांड लाइन आर्ग्यूमेंट्स पढ़ें कुछ ऐसे:

```rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

अगर आप इसे `cargo run` के साथ चलाते हैं, तो आपको कुछ ऐसा देखने को मिलेगा:

```
["path/to/program", "arg1", "arg2"]
```

## गहराई से जानकारी (Deep Dive)
पहले जमाने में, ज्यादातर प्रोग्राम टर्मिनल इंटरफेस से चलते थे, इसलिए कमांड लाइन आर्ग्यूमेंट्स बहुत अहम थे। Rust में `std::env` मॉड्यूल से आसानी से कमांड लाइन आर्ग्यूमेंट्स पढ़े जा सकते हैं। 

अलटरनेटिव में `clap` या `getopts` जैसे क्रेट्स भी हैं, जो और ज्यादा फंक्शनलिटी देते हैं। जहां `std::env::args` बेसिक उपयोग के लिए ठीक है, वहीं कॉम्प्लेक्स ऐप्लिकेशन के लिए `clap` या `getopts` बेहतर हो सकते हैं।

## संबंधित स्रोत (See Also)
- [The Rust Programming Language Book - Command Line Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
