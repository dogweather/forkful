---
title:                "Rust: स्टैंडर्ड त्रुटि पर लिखना"
simple_title:         "स्टैंडर्ड त्रुटि पर लिखना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Kyu Likhein Standard Error mei

## Kyu

Standard error ek aise programming concept hai jo bahut kam logon ke dimaag mein aata hai, lekin yeh bahut important hai. Jab hum kisi Rust program ko run karte hain, toh usme kuch errors ya bugs aa sakte hain. Jab yeh errors humare console screen par show hote hain, toh hume pata chalta hai ki kahaan kuch problem hai. Standard error mein likhne se hum apne program ke errors ko asaani se identify aur fix kar sakte hain.

## Kaise Karein

Standard error mein likhna bahut asaan hai. Sirf ek `println!()` statement ko `eprintln!()` mein change kar dena hai. Example ke liye, agar hum ye code likhte hain:

```Rust
println!("Hello world!");
```

Toh iska output console screen par print hoga. Lekin agar hum kuch error message ko standard error mein print karna chahte hain, toh hum ye code likhenge:

```Rust
eprintln!("Error: Value out of range!");
```

Is tarah se hum apne errors ko standard error mein print kar sakte hain.

## Gehri Jankari

Standard error ka use errors ko identify karne ke alawa hum debug karte samay bhi kar sakte hain. Jab hum apna program debug karte hain, toh hume step by step progress report ki jarurat hoti hai. Standard error mein likhne se hum apne program ke intermediate values ko dekh sakte hain aur sahi output tak pahunchne mein madad mil sakti hai.

Ek aur important baat hai ki standard error mein likhne se hum apne program ko user-friendly bana sakte hain. Jab hum errors ko console screen par print karte hain, toh wo bahut messy aur difficult to read ho sakta hai. Lekin standard error mein likhne se errors clear aur organised tarike se show hote hain.

# See Also

Agar aapko aur jyada jankari chahiye standard error ke baare mein, toh aap in links par click kar sakte hain:

- [Rust Standard Library Documentation](https://doc.rust-lang.org/std/io/fn.eprintln.html)
- [Understanding Standard Output, Error, and Redirection](https://www.geeksforgeeks.org/understanding-standard-output-error-redirection/)
- [Debugging Rust Programs with VS Code](https://code.visualstudio.com/docs/rust/rust-debugging)