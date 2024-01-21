---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:57:23.878537-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
कमांड लाइन आर्ग्यूमेंट्स पढ़ना मतलब आपकी प्रोग्राम को यूज़र इनपुट मिलना टाइपिंग के जरिए। ये ज़रूरी होता है जब आप यूज़र से डायनामिक डेटा लेना चाहते हो, जैसे फाइल नाम या सेटिंग्स।

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