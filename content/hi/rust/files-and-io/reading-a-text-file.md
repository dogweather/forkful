---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases:
- /hi/rust/reading-a-text-file/
date:                  2024-01-20T17:55:46.595369-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट फाइल पढ़ना उसमें मौजूद डेटा को निकालने की प्रक्रिया है। प्रोग्रामर्स इसे कॉन्फ़िगरेशन, डेटा एनालिसिस या बस इनपुट के रूप में उपयोग करने के लिए करते हैं।

## कैसे करें:
Rust में टेक्स्ट फाइल पढ़ने के लिए आप `std::fs` मॉड्यूल का उपयोग कर सकते हैं। यहाँ एक सिंपल उदाहरण है:

```Rust
use std::fs;

fn main() -> Result<(), std::io::Error> {
    let contents = fs::read_to_string("sample.txt")?;
    println!("File Contents:\n{}", contents);
    Ok(())
}
```

इस कोड में `sample.txt` नामक फाइल की सामग्री को पढ़ा जा रहा है और उसे कंसोल पर प्रिंट किया जा रहा है।

## गहराई से जानकारी:
फाइल पढ़ना एक ऐसी विशेषता है जो कई दशकों से प्रोग्रामिंग भाषाओं में मौजूद है। `std::fs` Rust में फाइल सिस्टम ऑपरेशंस के लिए स्टैंडर्ड लाइब्रेरी है। `read_to_string` फंक्शन फाइल की सामग्री को एक `String` में पढ़ता है, जो इसे मैनेज करना आसान बनाता है। विकल्प के रूप में, आप `std::io::BufRead` ट्रेट का इस्तेमाल करके फाइल को लाइन दर लाइन पढ़ सकते हैं।

डेटा को फाइल से पढ़ते समय त्रुटि हैंडलिंग महत्वपूर्ण होती है, और Rust आपको `Result` टाइप का इस्तेमाल करके इसे अच्छे से करने की सुविधा देता है।

## अन्य स्रोत:
- Rust दस्तावेज़ीकरण पर `std::fs` मॉड्यूल: [Rust std::fs](https://doc.rust-lang.org/std/fs/index.html)
- Rust बुक का 'फाइल्स और इनपुट/आउटपुट' अध्याय: [Rust Book: File I/O](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)
