---
title:                "Rust: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Varför skriva till standard error i Rust?

Att skriva till standard error kan vara användbart när man vill skicka ut felmeddelanden eller annan felsökning till användaren. Det är också vanligt att logga viktiga händelser eller information till standard error.

## Så här gör du det i Rust

För att skriva till standard error i Rust kan du använda "eprint!" och "eprintln!" makron. Dessa fungerar på samma sätt som "print!" och "println!" makron, men skriver till standard error istället för standard output.

```rust
fn main() {
    let name = "Sofia";
    eprintln!("Hello, {}!", name);
}
```

Output:
```
Hello, Sofia!
```

## Djupdykning

När du skriver till standard error i Rust, skickas outputen direkt till den enhet som är ansvarig för att hantera felmeddelanden och annan felsökning. Detta gör det lättare att separera vanlig output från felmeddelanden och gör det enklare att läsa och förstå felmeddelanden.

För att läsa input från standard error kan du använda "read_line" funktionen från standardbiblioteket "std::io". Denna funktion läser en rad från standard error och returnerar en sträng.

```rust
use std::io;

fn main() {
    println!("Skriv något till standard error:");
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Kunde inte läsa input");
    
    eprintln!("Du skrev: {}", input);
}
```

Output:
```
Skriv något till standard error:
Hello world!
Du skrev: Hello world!
```

## Se även

- [Rust `std::io` biblioteket](https://doc.rust-lang.org/std/io/index.html)
- [Ett enkelt exempel på att använda standard error i Rust](https://www.geeksforgeeks.org/add-a-data-field-to-a-given-genotype-in-r/)
- [En guide till Rusts standard bibliotek](https://blog.logrocket.com/a-tale-of-std-and-friends-in-rust/)