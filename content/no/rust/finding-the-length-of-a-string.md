---
title:                "Finn lengden på en streng"
html_title:           "Rust: Finn lengden på en streng"
simple_title:         "Finn lengden på en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

"Hva & Hvorfor?": Å finne lengden av en streng betyr å telle antall tegn i en tekststreng. Dette er viktig for programvare for å kunne behandle og manipulere tekst på en effektiv måte.

"Hvordan:": For å finne lengden av en streng i Rust, kan du bruke metoden ```len()``` som er tilgjengelig for alle strenger. Denne metoden returnerer antall bytes i strengen, noe som også tilsvarer antall tegn for ASCII-strenger. For eksempel:

```Rust
let my_string = "Hei";
let length = my_string.len();
println!("Lengden på strengen er: {}", length);
```

Dette vil skrive ut "Lengden på strengen er: 3" siden det er tre tegn i strengen "Hei". Merk at dette også inkluderer mellomrom.

"Dype Dykk": Funksjonen for å finne lengden av en streng i Rust har vært tilgjengelig siden språket ble lansert i 2010. Det finnes alternative metoder, som for eksempel å bruke en løkke til å telle antall tegn i strengen, men metoden ```len()``` er den anbefalte måten å gjøre det på.

```len()```-metoden er implementert ved å telle antall bytes i strengen, som fungerer bra for ASCII-strenger, men kan føre til feil for ikke-ASCII-strenger som bruker flere bytes per tegn. Dette kan løses ved å bruke metoden ```chars()``` som returnerer en iterator over hvert tegn i strengen, og deretter bruke metoden ```count()``` for å telle antall tegn i iterator-en.

"Se også": For mer informasjon om hvordan Rust behandler tekst og strenger, se Rust sin offisielle dokumentasjon: https://doc.rust-lang.org/stable/book/ch08-02-strings.html