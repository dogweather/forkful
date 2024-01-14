---
title:                "Rust: कम्प्यूटर प्रोग्रामिंग पर भाषा समझना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर भाषा समझना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Kyun

Agar aap ek Rust programmer hain aur command line arguments ki reading ke bare mein jaanna chahte hain, to yeh post aapke liye hai. Yahan hum aapko command line arguments ki importance aur isko kaise padha ja sakta hai, uske bare mein batayenge.

# Kaise Karein

Sabse pehle, hume command line arguments ko read karne ke liye `std::env` module ko use karna hoga. Iske baad hum `args()` function ko call kar sakte hain jisse hume ek iterator mil jayega jo command line arguments ko store karega. Is iterator se hum arguments ko access kar sakte hain.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("Total arguments passed: {}", args.len());

    // Printing each argument
    for arg in args {
        println!("{}", arg);
    }
}
```

**Output:**

```bash
Total arguments passed: 3
target/debug/my_program
Hello
World
```

Is code snippet mein humne `args()` function se iterator liya aur use `collect()` method se `Vec<String>` mein convert kiya. Iska output `args` variable mein store hua. Iske baad humne `args.len()` ki madad se total arguments ko print kiya. Iske baad hum `for` loop ka use karke har argument ko print kiya.

# Gehri Jhaank

Command line arguments reading ke alawa, `std::env` module hume aur bhi kai interesting methods provide karta hai. Jaise ki hum arguments ko filter kar sakte hain, specific arguments ko extract kar sakte hain, etc.

Ek interesting method hai `args_os()` jo Rust ka `OsString` type ke arguments ko return karta hai. Iska use UTF-8 encoding ke sath text ko handle karne ke liye kiya ja sakta hai.

# See Also

Kuch aur articles command line arguments ki reading ke bare mein:

- [Command Line Arguments in Rust](https://doc.rust-lang.org/std/env/fn.args.html)
- [Command Line Args using Env Module](https://www.geeksforgeeks.org/command-line-argument-in-rust/)
- [Reading Command Line Arguments in Rust](https://stackoverflow.com/questions/56826225/reading-command-line-arguments-in-rust)
- [Using Command Line Arguments in Rust Program](https://www.educative.io/edpresso/how-to-use-command-line-arguments-in-a-rust-program)