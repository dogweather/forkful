---
aliases:
- /sv/rust/concatenating-strings/
date: 2024-01-20 17:35:31.814330-07:00
description: "Sammanfogning av str\xE4ngar \xE4r processen att limma ihop textstycken\
  \ f\xF6r att skapa en ny, sammanh\xE4ngande text. Programmerare g\xF6r detta f\xF6\
  r att bygga\u2026"
lastmod: 2024-02-18 23:08:51.576228
model: gpt-4-1106-preview
summary: "Sammanfogning av str\xE4ngar \xE4r processen att limma ihop textstycken\
  \ f\xF6r att skapa en ny, sammanh\xE4ngande text. Programmerare g\xF6r detta f\xF6\
  r att bygga\u2026"
title: "Sammanslagning av str\xE4ngar"
---

{{< edit_this_page >}}

## Vad & Varför?
Sammanfogning av strängar är processen att limma ihop textstycken för att skapa en ny, sammanhängande text. Programmerare gör detta för att bygga textmeddelanden, sammansätta data eller generera kod dynamiskt.

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
