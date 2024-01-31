---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

लिखना टेक्स्ट फाइल डेटा को फाइल में सेव करने की प्रक्रिया है। प्रोग्रामर्स इसका उपयोग डेटा को संग्रहित करने, कॉन्फिग्युरेशन सेटिंग्स सेव करने और लॉग फाइल्स बनाने के लिए करते हैं।

## कैसे करें:

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut file = File::create("example.txt").expect("फाइल बनाने में समस्या आई।");
    file.write_all(b"नमस्ते, Rust!\n")
        .expect("फाइल में लिखने में समस्या आई।");
}
```
इस कोड से `example.txt` नामक नई फाइल बनेगी और उसमें "नमस्ते, Rust!" लिखा जाएगा।

## गहराई से जानकारी:

रस्ट में फाइल्स को सम्भालने का सिस्टम उसकी मेमोरी सेफ्टी प्रिंसिपल पर आधारित है। ऐतिहासिक रूप में, फाइल I/O एरर प्रोन था, परंतु Rust आपको कड़े टाइप चेक्स और एरर हैंडलिंग के माध्यम से अधिक सुरक्षा देता है। विकल्प में `write!` और `writeln!` मैक्रोस हैं, जो `Write` ट्रेट वाले किसी भी डेटा स्ट्रीम पर इस्तेमाल हो सकते हैं।

## देखें भी:

- Rust by Example का [File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html) सेक्शन।
- [std::fs](https://doc.rust-lang.org/std/fs/) मॉड्यूल का डॉक्युमेंटेशन।
- [std::io](https://doc.rust-lang.org/std/io/) मॉड्यूल के बारे में अधिक जानकारी।
