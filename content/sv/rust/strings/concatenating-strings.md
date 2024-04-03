---
date: 2024-01-20 17:35:31.814330-07:00
description: "S\xE5 h\xE4r g\xF6r du: Du kan sammanfoga str\xE4ngar i Rust p\xE5 n\xE5\
  gra olika s\xE4tt. Anv\xE4nd `+` operatorn eller `format!` makrot f\xF6r att smidigt\
  \ kombinera dem."
lastmod: '2024-03-13T22:44:37.689816-06:00'
model: gpt-4-1106-preview
summary: "Du kan sammanfoga str\xE4ngar i Rust p\xE5 n\xE5gra olika s\xE4tt."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## Så här gör du:
Du kan sammanfoga strängar i Rust på några olika sätt. Använd `+` operatorn eller `format!` makrot för att smidigt kombinera dem.

```Rust
fn main() {
    let greeting = "Hej".to_string();
    let target = "världen";
    let exclamation = "!";

    // Använder `+`
    let mut message = greeting + " " + target + exclamation;
    println!("{}", message);  // Utmatning: Hej världen!

    // Använder `format!`
    message = format!("{} {}{}", greeting, target, exclamation);
    println!("{}", message);  // Utmatning: Hej världen!
}
```

## Djupdykning
Förr i tiden, när datorer hade mindre minne, var det viktigt att hantera strängar effektivt. Rust värdesätter säkerhet och prestanda, så det erbjuder olika sätt att sammanfoga strängar. Använd `+` operatören för enklare sammanfogningar, men observera att den tar ägandet över den första strängen. `format!` är mer flexibelt men kan vara långsammare eftersom det hanterar minnesallokeringar under körningen. Det är viktigt att förstå äganderätt ('ownership') och lånecheckning ('borrow checking') för att förhindra minnesproblem.

## Se även
- Rust officiella dokumentation om strängar: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- "The Rust Programming Language" boksekvens om minneshantering: [https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)
