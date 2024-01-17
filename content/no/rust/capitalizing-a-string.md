---
title:                "Stor bokstav i en streng"
html_title:           "Rust: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Når vi snakker om å "capitalize" en streng, betyr det at vi gjør den første bokstaven i strengen stor, mens resten av bokstavene forblir små. Dette er en vanlig praksis blant programmører for å gjøre kode mer lesbar og for å følge standard konvensjoner.

# Slik gjør du:

```Rust
let streng = "hallo";
let capitalized = streng.to_uppercase();
println!("{}", capitalized);
```

Dette vil resultere i utskriften "HALLO". Vi bruker metoden `to_uppercase()` på strengen `hallo` for å konvertere den til en ny streng med stor forbokstav.
Dette er et enkelt eksempel, men du kan også bruke en løkke for å gjøre den første bokstaven i en setning stor.

```Rust
let setning = "dette er en setning";
let splittet: Vec<&str> = setning.split(" ").collect();
for ord in splittet {
    print!("{} ", ord.to_uppercase());
}
```

Denne koden deler setningen opp i en `Vec` og bruker en løkke for å gjøre hver første bokstav i hvert ord stor. Dette vil resultere i utskriften "Dette Er En Setning".

# Dypdykk:

Capitalize-metoden stammer fra metoden `capitalize()` i programmeringsspråket Perl. Det finnes også andre måter å gjøre en streng til en stor forbokstav på, som for eksempel`ucfirst()` og `mb_convert_case()`.

I tillegg til å konvertere en streng til å ha en stor forbokstav, kan du også konvertere en streng til å ha alle små bokstaver ved å bruke `to_lowercase()`.

# Se også:

- Offisiell Rust dokumentasjon for `to_uppercase()` [Rust Docs](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- Kodeeksempel for å gjøre den første bokstaven i et ord stor [Stack Overflow](https://stackoverflow.com/questions/44831930/how-can-i-capitalize-the-initials-of-each-word-in-a-sentence-in-rust)