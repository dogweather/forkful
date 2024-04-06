---
date: 2024-01-20 17:41:20.003724-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0938\u0948\u0902\
  \u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:53.994355-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

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
