---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Regular expressions, eller regex, är mönster för att matcha textsträngar. Programmerare använder dem för att söka, validera, eller redigera text effektivt.

## Hur gör man?
I Rust använder vi `regex`-cratet. Här är ett exempel på sökning:

```Rust
use regex::Regex;

fn main() {
    let text = "Hitta nummer: 12345.";
    let re = Regex::new(r"\d+").unwrap();

    let cap = re.find(text).unwrap();
    println!("Hittat: {}", &cap.as_str());
}
```

Output:
```
Hittat: 12345
```

Och för att ersätta text:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s+\1\b").unwrap();
    let result = re.replace_all("hej hej världen", "hej");

    println!("Uppdaterad text: {}", result);
}
```

Output:
```
Uppdaterad text: hej världen
```

## Fördjupning
Regex har sitt ursprung i teoretisk datavetenskap och automatteori från 1950-talet. Alternativ till regex innefattar strängmanipulationsfunktioner och parser kombinator-bibliotek. I Rust implementeras regex med en bakomliggande deterministisk ändlig automaton (DFA) för prestanda och säkerhet.

## Se även
- [Rust Regex Crates dokumentation](https://docs.rs/regex/)
- [Rust bokkapitel om mönstermatchning](https://doc.rust-lang.org/book/ch18-00-patterns.html)
- [Playground för att testa Rust kod](https://play.rust-lang.org/)
- [Artikel om komplexitet av regex-engines](https://swtch.com/~rsc/regexp/regexp1.html)
