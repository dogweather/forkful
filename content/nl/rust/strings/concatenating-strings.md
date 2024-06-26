---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:31.504079-07:00
description: 'Hoe: Rust biedt je een paar manieren om teksten samen te voegen. Laten
  we het eens bekijken.'
lastmod: '2024-03-13T22:44:50.586068-06:00'
model: gpt-4-0125-preview
summary: Rust biedt je een paar manieren om teksten samen te voegen.
title: Samenvoegen van strings
weight: 3
---

## Hoe:
Rust biedt je een paar manieren om teksten samen te voegen. Laten we het eens bekijken.

### Gebruikmakend van de `+` Operator
```Rust
let hello = "Hello".to_string();
let world = " world!";
let result = hello + world;
println!("{}", result); // Output: Hello world!
```
De `+` kleeft `" world!"` aan `"Hello"`, maar let op, `hello` moet een `String` zijn, geen slice.

### De `format!` Macro
```Rust
let mood = "happy";
let message = format!("Heb een {} dag!", mood);
println!("{}", message); // Output: Heb een happy dag!
```
`format!` lijkt op `println!`, door variabelen in tekst te mengen. Super handig voor sjablonen.

### Toevoegen aan een String
```Rust
let mut tip = "Remember to".to_string();
tip.push_str(" ademhale.");
println!("{}", tip); // Output: Remember to breathe.
```
`push_str` voegt een slice toe aan een `String`. Goed voor het stukje bij beetje toevoegen.

## Diepere Duik
Stringconcatenatie is geen nieuw concept. Het bestaat al sinds het begin van het programmeren; tenslotte moesten we altijd al woorden samenvoegen.

In Rust is een `String` een groeiende, veranderlijke, eigenaar zijnde UTF-8 gecodeerde string type. Er zijn alternatieven zoals `&str`, een string slice, wat een kijkje in een `String` is.

Elke methode heeft zijn compromissen:

- `+` operator is snel voor een of twee samenvoegingen maar verbruikt de linkeroperand (het neemt eigendom). Elke `+` alloceert ook geheugen, wat kan oplopen.
  
- `format!` grijpt geen eigendom waarden, wat beleefd is, maar het kan langzamer zijn vanwege zijn flexibiliteit en het toewijzen voor elke aanroep. Het is je Zwitsers zakmes voor stringassemblage.

- `push_str` is efficiënt voor een herhaalde toevoeging. Het alloceert niet tenzij de `String` meer ruimte nodig heeft.

Rust's focus op eigendom en lenen betekent dat het strings een beetje anders behandelt dan talen zoals Python of JavaScript. Dit verschil zorgt voor geheugelveiligheid maar kan ook komen met een leercurve.

## Zie Ook
Voor een diepere duik:
- Rust Boek over Strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust per Voorbeeld over Strings: https://doc.rust-lang.org/rust-by-example/std/str.html
- Rust std::string::String API Documentatie: https://doc.rust-lang.org/std/string/struct.String.html
