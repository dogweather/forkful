---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कमांड लाइन आर्गुमेंट्स पढ़ना मतलब होता
है की हमारे प्रोग्राम को युज़र्स कमांड 
लाइन से क्या इनपुट दे रहे हैं उसे जानना। 
यह इसलिए किया जाता है ताकि हमारा प्रोग्राम फ्लेक्सिबल 
हो सके।

## कैसे:

Rust में, `std::env::args` और `std::env::args_os` ये
दो फंक्शन होते हैं जो हमें कमांड लाइन आर्गुमेंट्स 
देते हैं।

```Rust
fn main() {
    let args: Vec<String> = std::env::args().collect();
    for arg in args {
        println!("{}", arg);
    }
}
```
फिर इसे कमांड लाइन से `rustc program.rs && ./program hello world` ऐसे इस्तेमाल कर सकते हो:

```
./program
hello
world
```

## गहरा अध्ययन

#### ऐतिहासिक संदर्भ:

Rust भाषा का शुरू किया गया था इसी विचार के साथ की इसमें सेफ्टी (safety), स्पीड (speed), और सिमल्टेनियस एब्स्ट्रैक्शन (simultaneous abstraction) दिया जाए।

#### विकल्प:

Rust के पास `std::env::args` और `std::env::args_os` के अलावा और भी लाइब्रेरीज़ हैं - जैसे `docopt`, `getopt`, `clap` जिससे हम command line पार्सिंग कर सकते हैं। यह एडवांस्ड काम के लिए होते हैं।

#### कार्यान्‍वयन विवरण:

Rust में `std::env::args` और `std::env::args_os` इनका इस्तेमाल तभी करना चाहिए जब आपको सरल आर्गुमेंट्स चाहिए हों। यदि आपके पास बड़ी प्रोजेक्ट है तो `docopt`, `getopt`, `clap` जैसे लाइब्रेरी का इस्तेमाल करना चाहिए।

## और भी देखें

1. [Rust गाइड](https://doc.rust-lang.org/stable/book/)
2. [`std::env` डॉक्यूमेंटेशन](https://doc.rust-lang.org/std/env/)
3. [`docopt.rs`](https://github.com/docopt/docopt.rs)
4. [`getopt`](https://github.com/rust-lang/getopts)
5. [`clap`](https://github.com/clap-rs/clap)