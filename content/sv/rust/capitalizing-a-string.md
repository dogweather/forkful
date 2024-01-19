---
title:                "Gör om en sträng till versaler"
html_title:           "Rust: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Att kapitalisera en sträng i Rust
Informationen och koden i denna artikel riktar sig till programmerare som använder det senaste Rust (den nuvarande versionen).

## Vad & Varför?
Att kapitalisera en sträng innebär att första bokstaven i en textsträng omvandlas till en versal. Detta är användbart för att standardisera textdata, exempelvis vid formattering av namn.

## Hur gör man:
Att kapitalisera en sträng i Rust är relativt enkelt. Här är en kodsnutt och dess resuultat:

```Rust
fn main() {
    let my_string = "hej världen!";
    let capitalized_string = my_string.chars().enumerate().map(|(i,c)| 
        if i == 0 {c.to_uppercase().to_string()} else {c.to_string()}).collect::<String>();
    println!("{}", capitalized_string);
}
```

När du kör ovanstående skript får du:

```
"HEJ VÄRLDEN!"
```

Detta är en enkel men effektiv lösning för att kapitalisera första bokstaven i en sträng.

## Djupare dykning
Det är viktigt att notera att Rusts .to_uppercase() inte alltid beter sig som förväntat med icke-engelska tecken. Detta är ett känt problem och har att göra med hur tecken som ligger utanför ASCII-standarden hanteras.

Ett alternativ till att använda .to_uppercase() är att använda biblioteket unicase som hanterar Unicode-strängar mer tillförlitligt.

Vid det här laget kanske du undrar varför vissa programmerare väljer att kapitalisera sina strängar manuellt istället för att använda en inbyggd funktion som .to_uppercase(). Faktum är att det faktiskt kan vara snabbare och mer effektivt att göra det på egen hand, särskilt om strängens längd varierar mycket.

## Se också
Om du vill veta mer om att arbeta med strängar i Rust, rekommenderas följande resurser:

1. [Rusts officiella dokumentation om strängar](https://doc.rust-lang.org/book/ch08-02-strings.html)
2. [Rust by Example: Strängar](https://doc.rust-lang.org/rust-by-example/std/str.html)
3. [StackOverflow tråd om kapitalisering i Rust](https://stackoverflow.com/questions/38406793/why-is-there-no-capitalize-function-in-rust)