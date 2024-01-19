---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Stringssammanfogning är processen att kombinera två eller flera strängar (med text) till en. Programmerare gör detta för att manipulera eller omorganisera information på ett mer meningsfullt och effektivt sätt.

## Så här gör man:
Se på följande exempel med hjälp av + och format!-makro.

```Rust
let s1 = "Hej".to_string();
let s2 = " värld".to_string();

// Metod 1: använda +
let s3 = s1 + &s2;
println!("{}", s3); // Output: Hej värld

// Metod 2: använda format!-makro
let s4 = format!("{}{}", "Hej", " värld");
println!("{}", s4); // Output: Hej värld
```

Observera: Använd plus-operatören endast när du vill lägga till en sträng till en annan. Lägga till många strängar skapar oroligt kod och stora prestandakostnader. 

## Djupdykning

### Historisk kontext
Konceptet att sammanfoga strängar har funnits sedan begynnelsen av programmeringshistoria. Men tillvägagångssättet och genomförandet har varierat beroende på olika programmeringsspråk och deras filosofier.

### Alternativ
I Rust kan strängar också fogas samman med `push_str`-metoden. 

```Rust
let mut s5 = "Hej".to_string();
s5.push_str(" värld");
println!("{}", s5); // Output: Hej värld
```

### Interna detaljer
När vi arbetar med sammanfogning av strängar i Rust, är det viktigt att förstå att "String" och "tuppligt sträng" är olika typer i Rust. I de flesta fall omvandlas "tuppligt sträng" till en "String" för att utföra sammanfoga operationer, vilket kan medföra en prestandakostnad på grund av de extra minnestilldelningarna.

## Se även

Se följande länkar för ytterligare läsning: 

1. [String Vs Str i Rust](https://www.ameyalokare.com/rust/2017/10/12/rust-str-vs-String.html)
2. [Effektiv strängkonkatenering i Rust](https://fasterthanli.me/articles/efficient-string-concatenation-in-rust)
3. [Rust String API Dokumentation](https://doc.rust-lang.org/std/string/struct.String.html)