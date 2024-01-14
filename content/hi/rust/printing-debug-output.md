---
title:    "Rust: डिबग आउटपुट प्रिंट करना"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Kyun

Debug output print karne ka matalab hota hai ki hum apne code me koi problem solve karne ka prayas kar rahe hai. Isse hum apne code ke samasye ka samadhan khoj sakte hai aur behtar code likh sakte hai.

## Kaise Karein

Agar aap apne Rust project me debug output print karna chahte hain, to aap ye steps follow kar sakte hain:

```Rust
fn main() {
  // `println!` macro ka use karke hello world print karein
  println!("Hello World!");
}
```

Ab hum is code ko compile karke run karenge, to humein output me `Hello World!` milega.

Agar hum variables ya khali string print karna chahte hain, to hum ye code ka use kar sakte hain:

```Rust
fn main() {
  let name = "John";
  // `println!` macro ka use karke variable print karein
  println!("My name is {}", name);
}
```

Is output me humein `My name is John` milega.

Is tarah se hum apne code me debug output print kar sakte hain aur apne code ki samasyao ka samadhan nikal sakte hain.

## Gehri Jankari

Debug output print karne ke liye Rust me hum `println!` macro ka use karte hain. Ye macro humein string, variables ya expressions ka output print karne me madad karta hai.

Hum `println!` ke sath dusre options bhi use kar sakte hain jaise ki `eprintln!` (standard error stream me print karta hai), `print!` (newline ki jaga space use karta hai), `eprint!` (standard error stream me space ki jaga print karta hai).

Iske alawa, hum `format!` macro ka bhi use kar sakte hain jise hum apne code me custom formatting ke liye use kar sakte hain.

## See Also

Is article ke padhne ke bad, aap Rust ke debug output print karne ke bare me puri tarah samajh gaye hain. Agar aap aur jankari chahte hain toh, aap in resources par visit kar sakte hain:

- Official Rust documentation on formatting: https://doc.rust-lang.org/std/fmt/
- Rust by Example: Debugging and Printing: https://doc.rust-lang.org/stable/rust-by-example/hello/print.html