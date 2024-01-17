---
title:                "Att använda reguljära uttryck"
html_title:           "Rust: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Användning av reguljära uttryck, även kallat regex, är en vanlig praxis bland programmerare för att söka och manipulera textsträngar. Det är ett kraftfullt verktyg som kan användas för att identifiera mönster och utföra sök- och ersättningsoperationer på text.

## Så här gör du:
```Rust
use regex::Regex;

fn main() {
    // Skapa ett nytt reguljärt uttryck för att söka efter "hello world"
    let re = Regex::new(r"hello world").unwrap();

    // Skapa en textsträng att söka igenom
    let text = "Hello world, detta är en exempelsträng";

    // Utför sökningen och skriv ut resultatet
    println!("{}", re.find(text).unwrap().as_str()); // Output: hello world
}
```

```Rust
use regex::Regex;

fn main() {
    // Skapa ett nytt reguljärt uttryck för att ersätta "hej" med "hello"
    let re = Regex::new(r"hej").unwrap();

    // Skapa en textsträng att utföra ersättningen på
    let text = "Hej alla, vad gör ni?";

    // Utför ersättningen och skriv ut resultatet
    println!("{}", re.replace_all(text, "hello")); // Output: hello alla, vad gör ni?
}
```

## Djupdykning:
Reguljära uttryck har funnits sedan 1950-talet och används i många programmeringsspråk, inte bara i Rust. Alternativ till regex inkluderar strängmanipuleringsfunktioner och parseringsbibliotek.

Implementationen av regex i Rust är byggd på NFA (Non-deterministic Finite Automata) och erbjuder hög prestanda och korrekthet för komplexa mönster.

## Se även:
- [Rust regex dokumentation](https://docs.rs/regex/1.5.4/regex/)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)