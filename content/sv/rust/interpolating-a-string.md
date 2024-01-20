---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolerad strängning innebär att infoga variabler eller uttryck inom en sträng. Programmerare gör detta för att skapa dynamiska strängar och förhindra onödiga konkatenationer.

## Hur man gör:

Rust tillåter stränginterpolation genom att använda `format!` makroet. Här är ett exempel:

```Rust
fn main() {
    let name = "Daniel";
    let age = 30;
    let presentation = format!("Hej, jag heter {} och jag är {} år gammal", name, age);
    
    println!("{}", presentation);
}
```

I konsolen ser du:

```
Hej, jag heter Daniel och jag är 30 år gammal
```

## Djupdykning 

Stränginterpolation har funnits i andra språk som Ruby och JavaScript långt innan Rust införde `format!`. I Rust används dock en starkt typad stränginterpolation för att undvika fel vid runtime.

Alternativt kan du använda `println!` direkt för interpolation om resultatet ska skrivas ut. Men `format!` är mer flexibel eftersom det returnerar den interpolerade strängen.

När det gäller implementeringsdetaljer är det värt att nämna att `format!` använder ett komplex serie av makron för att uppnå detta, och fungerar endast på litterära strängar, inte strängvariabler.

## Se også 

För vidare utveckling, kolla in dessa källor:

1. ["The Rust Programming Language" Bok](https://doc.rust-lang.org/book/title-page.html) - Här finns detaljerade om rust programmering inklusive stränginterpolation och mer. 

2. [Rust´s Official Documentation on "format!" Macro](https://doc.rust-lang.org/std/fmt/) - Detaljerad information om 'format!' makroet, inkluderar exempel och olika användningsfall. 

3. [Rust by Example](https://doc.rust-lang.org/rust-by-example/index.html) - Detta är en samling praktiska exempel för att hjälpa dig att lära dig Rust programmering.