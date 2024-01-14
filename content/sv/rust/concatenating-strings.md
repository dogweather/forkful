---
title:    "Rust: Sammanslagning av strängar"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar är en vanlig uppgift vid programmering som ofta används för att skapa dynamiska och läsbara utskrifter. I Rust finns det flera olika sätt att sammanfoga strängar och det är viktigt att förstå de olika metoderna för att kunna skriva effektiv kod.

## How To

För att sammanfoga strängar i Rust finns det två huvudsakliga metoder: `format!` och `to_string`. Båda metoderna tar emot en eller flera strängar som argument och returnerar en ny sträng som är en kombination av de inmatade strängarna.

```Rust
// Metod 1: Använda format!
let name = "Sven";
let age = 30;
let introduction = format!("Hej, jag heter {} och jag är {} år gammal.", name, age);
println!("{}", introduction);
// Output: Hej, jag heter Sven och jag är 30 år gammal.

// Metod 2: Använda to_string
let text = "Det är ".to_string() + "en fin dag.";
println!("{}", text);
// Output: Det är en fin dag.
```

I det första exemplet använder vi funktionen `format!` för att kombinera variablerna `name` och `age` med en statisk text och skapa en komplett introduktion. I det andra exemplet använder vi metoderna `to_string` och `+` operatorn för att sammanfoga två strängar.

## Deep Dive

Vid fördjupning av detta ämne är det viktigt att förstå att `format!`-metoden är lämplig när man behöver skapa en sträng med variabler eller annan dynamisk data. Den kan även formatera data på ett specifikt sätt, till exempel som en valuta eller ett datum. Det är därför en vanlig metod för att skapa utskrifter i konsolen eller på webbsidor.

Å andra sidan är `to_string`-metoden mer användbar när man behöver en enkel kombination av två statiska strängar. Det kan också vara snabbare än `format!`-metoden eftersom det inte behöver genomföra någon formatering av data.

## Se även

Här är några användbara länkar för att lära dig mer om att sammanfoga strängar i Rust:

- [Rust documentation on strings](https://doc.rust-lang.org/std/string/)
- [Rust by Example - strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust programming book - strings](https://doc.rust-lang.org/book/ch08-02-strings.html)