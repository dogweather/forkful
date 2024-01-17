---
title:                "एक पाठ फाइल लिखना"
html_title:           "Rust: एक पाठ फाइल लिखना"
simple_title:         "एक पाठ फाइल लिखना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Kya Aur Kyu?
Text file ko likhna ek aam kaam hai jo kisi bhi programming language me kiya ja sakta hai. Text file me hum text ko store kar sakte hain, jaise hum notepad me karte hain. Programmers text file ko likhte hain apne code ko store karne ke liye ya phir data ko save karne ke liye.

## Kaise:
Coding ke dwaara hum text file ko Rust me aasan tareeke se likh sakte hain. Neeche diye gaye code blocks me diye gaye examples ko try karein aur output ko dekhein.

```
// Ek text file ko create karne ke liye, is tarah se "file.txt" file banana hoga

use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::create("file.txt").expect("Unable to create file");
    file.write_all(b"Hello World!").expect("Unable to write data");
}
```

```
// File ko padhne ke liye, is tarah se karenge

use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("file.txt").expect("Unable to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Unable to read data");
    print!("{}", contents); // Output: Hello World!
}
```

## Gehre Adhyayan
Text file ko likhne ka prachalan kayi saalon se hai. Isme hum apne data ko ek simple aur readable format me store kar sakte hain. Kai baar, text file me store kiya gaya code ko doosre programming languages me bhi easily convert kiya ja sakta hai. Isse humare code ka portability badhta hai.

## See Also:
- https://www.hackerearth.com/practice/notes/understanding-input-output-in-rust/ - Rust me input aur output ke baare me adhik jaankari.
- https://doc.rust-lang.org/std/fs/struct.File.html - File handling documentation for Rust.
- https://opensource.com/article/19/5/reading-writing-files-rust - Rust me file handling tutorial.