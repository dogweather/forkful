---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "Rust: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Kyun

Kya aapne kabhi temporary file ka istemal kiya hai? Agar nahi, toh aapko sochne ki zaroorat hai. Temporary files programming mein kaafi mahatvapurna hote hain. Ye file temporary taur par banaayi jaati hain aur kisi specific task ke liye istemal ki jaati hain. Rust mein, temporary files create karne ka process kaafi easy hai aur isme kuch alag interesting features bhi hain jinse aapko fayda ho sakta hai.

## Kaise Karein

Temporary files create karne ke liye, aapko "tempfile" crate ko use karke ek naya temporary file object create karna hoga. Iske baad, aap "tempfile::Builder" ka use karke file ka naam aur location define kar sakte hain. Iss builder object ke baad, hume file ko open aur write karne ke liye "File" object ki zaroorat hogi. Yeh process kuch is tarah se hai:

```Rust
use tempfile::Builder;

fn main() {
    let file = Builder::new()
        .prefix("tempfile")
        .tempfile()
        .expect("Failed to create temporary file");

    println!("File name: {}", file.path().display());
    
    // File operations can be performed here
    
}
```

Is code block mein humne "tempfile" crate ko import kiya hai aur Builder object create kiya hai. Iske baad, humne builder object ko use karke file ka naam set kiya aur temp file create kiya. Iske baad, hum file ka naam display kar sakte hain. Aap file operations ko kuch is tarike se perform kar sakte hain:

```Rust
use tempfile::NamedTempFile;

fn main() {
    let mut file = NamedTempFile::new().expect("Failed to create temporary file");

    // Write operations
    file.write_all(b"Hello World!").expect("Failed to write to file");

    // Read operations
    file.read_to_string(&mut contents).expect("Failed to read file");

    println!("File contents: {}", contents);
}
```

Is code block mein humne "tempfile" crate ko import kiya hai aur NamedTempFile object create kiya hai. Iske baad, hum ne file mein data write aur read kiya hai. Aap isme koi bhi file operations perform kar sakte hain jaise read, write ya delete.

## Gehri Jhaank

Temporary files create karne ka process kaafi simple hai lekin iski peechhe ka logic kaafi gehra aur important hai. Temporary files crate mein, ek unique file name generate karne ka algorithm hai jo duplicate file names se bachata hai. Iske alawa, ye process kuch safety precautions bhi follow karta hai jaise file deletion aur overwrite prevention.

## Dekhein Bhi

Agar aapko temporary files ke baare mein aur jaankaari chahiye ya aapko base libraries aur crates ke baare mein aur jaankaari chahiye, toh aap neeche diye gaye links check kar sakte hain:

- [Rust Programming Language](https://www.rust-lang.org/)
- [Official Rust Documentation](https://doc.rust-lang.org/)
- [Tempfile Crate Documentation](https://docs.rs/tempfile/latest/tempfile/)
- [Standard Library Documentation](https://doc.rust-lang.org/std/index.html)