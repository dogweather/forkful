---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# **Sök och ersätt text i Rust: En snabb guide**

## **Vad & Varför?**

I programmering, söker och ersätter vi text för att trasiga, felstavade eller oönskade teckensträngar kan omvandlas till korrekta eller önskade. Denna teknik är oumbärlig för dataframställning och -rensning.

## **Hur Man Gör:**

Här är ett grundläggande exempel på hur vi använder "str::replace" funktionen i Rust:

```Rust
fn main() {
    let gammal_text = "Hej, jag heter Sekret.";
    let ny_text = gammal_text.replace("Sekret", "Mysterium");
    println!("{}", ny_text);
}
```

Utförandet av ovanstående kod ger:

```
Hej, jag heter Mysterium.
```

## **Djupdykning:**

*Historisk kontext:* Tekniken för att söka och ersätta text i programmering har dess rötter i tidiga textbehandlingsprogram som Sed och Emacs. Rusts metoder för textmanipulation är mycket inspirerade av dessa historiska verktyg, men med en modern twist.

*Alternativ:* Förutom `str::replace`, Rust tillhandahåller också "str::replacen" för att ersätta ett visst antal förekomster, och `str::chars` tillsammans med `collect::<String>()` för att göra mer invecklade substitutioner.

*Implementeringsdetaljer:* `str::replace` metoden i Rust söker igenom texten enligt mängden som ges, och byter ut varje förekomst av det gamla värdet med det nya värdet. Detta sker igenom varje karaktär i strängen, vilket gör operationen följdriktig men möjligtvis en aning långsammare för stora mängder text.

## **Se Även:**

För mer detaljerad information, se de officiella dokumenten:

1. [Rust Docs: str::replace](https://doc.rust-lang.org/std/primitive.str.html#method.replace)
2. [Rust Docs: str::replacen](https://doc.rust-lang.org/std/primitive.str.html#method.replacen)
3. [Rust Docs: str::chars](https://doc.rust-lang.org/std/primitive.str.html#method.chars)