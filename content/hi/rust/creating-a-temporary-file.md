---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:41:20.003724-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
अस्थायी फाइल बनाना एक ऐसी प्रक्रिया है जिसमें डेटा को कुछ समय के लिए स्टोर करने के लिए एक फाइल बनाई जाती है। प्रोग्रामर यह तब करते हैं जब उन्हें डेटा को अस्थायी रूप से संग्रहित करने की जरूरत होती है, जैसे कि टेम्पोररी कैश, या फाइल सिस्टम आधारित लॉकिंग के लिए।

## कैसे करें:
```rust
use std::fs::File;
use std::io::{self, Write};
use tempfile::NamedTempFile;

fn main() -> io::Result<()> {
    let mut temp_file = NamedTempFile::new()?;
    writeln!(temp_file, "यह एक अस्थायी फाइल है")?;
    
    // अस्थायी फाइल का पथ दिखाएं
    println!("अस्थायी फाइल बनाई गई: {:?}", temp_file.path());
    
    Ok(())
}
```
सैंपल आउटपुट:
```
अस्थायी फाइल बनाई गई: "/tmp/.tmpA12B3C4"
```

## गहराई से जानकारी:
अस्थायी फाइलें यूनिक्स सिस्टम्स में विशेष '/tmp' डायरेक्टरी में रखी जाती हैं और अक्सर एक यूनिक नाम से पहचानी जाती हैं। Rust में, `tempfile` क्रेट का प्रयोग करके आप सुरक्षित रूप से अस्थायी फाइलें बना सकते हैं। वैकल्पिक रूप से `std::env::temp_dir` फंक्शन के द्वारा भी ऐसा कर सकते हैं, लेकिन `tempfile` क्रेट ज्यादा आसानी और कार्यक्षमता प्रदान करता है। Rust के मानक पुस्तकालय में अस्थायी फाइल और डायरेक्टरी बनाने की अच्छी सुविधा है जो ऑपरेटिंग सिस्टम के संसाधनों को सुरक्षित रूप से प्रबंधित करती है।

## संबंधित स्रोत:
- Rust `tempfile` crate: [docs.rs/tempfile](https://docs.rs/tempfile/)
- Rust `std::fs::File` documentation: [doc.rust-lang.org/std/fs/struct.File.html](https://doc.rust-lang.org/std/fs/struct.File.html)
