---
title:                "Rust: Sammanslagning av strängar"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar, eller concatenating strings på engelska, är en grundläggande del av programmering. Det är en viktig färdighet att ha i många olika program, särskilt när det kommer till att hantera text och data som användaren matar in.

## Så här gör du

För att sätta samman två strängar i Rust, kan du använda sig av "```Rust str1 + str2 ```" eller "```Rust format!(str1, str2) ```" beroende på ditt ändamål.

**Exempel:**

```Rust
let str1 = "Hej";
let str2 = "världen!";
let concatenated = str1 + str2;
println!("Resultatet blir: {}", concatenated);
```

Output:

```Rust 
Resultatet blir: Hej världen!
```

**Annat exempel:**

```Rust
let number = 10;
let str = "är mitt favorittal.";
let concatenated = format!("{} {}", number, str);
println!("Resultatet blir: {}", concatenated);
```

Output:
```
Resultatet blir: 10 är mitt favorittal.
```

## Djupdykning

I Rust så måste båda strängarna som sammanslås vara av samma typ, det vill säga att båda måste antingen vara `str` eller `String`. Detta är för att Rust är ett språk med statisk typning, vilket betyder att typerna på variablerna måste vara kända vid kompileringstid.

En annan sak att tänka på är att sammanslåenden av strängar kan göra att prestandan påverkas om det görs i en loop eller på en stor mängd data. Detta beror på att vid varje iteration eller sammanfogning så skapas en ny sträng, vilket kan bli resurskrävande. Det är därför viktigt att optimera sin kod när det kommer till att hantera stora datamängder.

## Se även

För mer information om att hantera strängar i Rust, kolla in dessa länkar:

- [The Rust Programming Language](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Concatenating strings in Rust](https://www.ameyalokare.com/rust/2017/10/12/rust-basics-of-concatenting-strings.html)
- [String concatenation performance in Rust](https://stackoverflow.com/questions/27004792/string-concatenation-in-rust)