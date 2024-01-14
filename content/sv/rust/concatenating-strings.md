---
title:    "Rust: Sammanslagning av strängar"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar, eller "concatenating strings" på engelska, är en vanlig operation inom programmering. Genom att sammanslå flera strängar till en enda kan man skapa mer flexibla och dynamiska applikationer.

## Hur man gör

För att sammanfoga strängar i Rust kan man använda sig av operatorn `+` eller funktionen `format!()`. Till exempel, om vi har två strängar `hej` och `världen`, kan vi använda `+` för att sammanfoga dem tillsammans:

```Rust
let hej = "hej";
let världen = "världen";

let resultat = hej + världen;

println!("{}", resultat); // output: hejvärlden
```

Man kan också använda `format!()` för att sammanfoga flera strängar med hjälp av platshållare `{}`:

```Rust
let hej = "hej";
let världen = "världen";

let resultat = format!("{} {}", hej, världen);

println!("{}", resultat); // output: hej världen
```

## Djupdykning

I Rust är strängar representerade som en sekvens av bytes, och att sammanfoga strängar innebär att man slår ihop dessa sekvenser. Detta kan dock påverka prestandan om man har många strängar som behöver sammanfogas. För att optimera detta kan man istället använda sig av `[String::from()]`, som kopierar innehållet av den första strängen och lägger till resten av strängarna till den, istället för att skapa en helt ny sträng från början.

## Se även

- Officiell Rust documentation för strängar: [https://doc.rust-lang.org/std/string/](https://doc.rust-lang.org/std/string/)
- En guide för att hantera strängar i Rust: [https://www.tutorialspoint.com/rust/rust_working_with_strings.htm](https://www.tutorialspoint.com/rust/rust_working_with_strings.htm)