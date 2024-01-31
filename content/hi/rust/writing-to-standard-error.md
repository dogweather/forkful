---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
मानक त्रुटि (standard error) में लिखना प्रोग्राम की गलतियों और अन्य जानकारी को लॉग करने के लिए होता है। प्रोग्रामर्स इसका उपयोग डीबगिंग में और उपयोगकर्ता को सिस्टम की विफलताओं की सूचना देने के लिए करते हैं।

## How to: (कैसे करें:)
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "गलती: फाइल नहीं खुल सकती।").expect("stderr में लिखने में असमर्थ।");
}
```
सैम्पल आउटपुट:
```
गलती: फाइल नहीं खुल सकती।
```

## Deep Dive (गहराई से जानकारी)
मानक त्रुटि स्ट्रीम (stderr) UNIX सिस्टमों पर अस्तित्व में आया था और इसका मुख्य उद्देश्य है कि गलतियों को मानक आउटपुट स्ट्रीम (stdout) से अलग किया जा सके। इसके विकल्पों में लॉग फाइल्स, ग्राफिकल यूजर इंटरफेस में एरर डायलॉग्स या नेटवर्किंग सोल्यूशंस शामिल हैं। Rust में, `std::io::stderr` का उपयोग करके सिम्पली stderr स्ट्रीम में लिखा जा सकता है।

## See Also (इसे भी देखें)
- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [Rust Documentation for `std::io::stderr`](https://doc.rust-lang.org/std/io/struct.Stderr.html)
