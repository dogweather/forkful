---
title:                "Utvinning av substringer"
html_title:           "Rust: Utvinning av substringer"
simple_title:         "Utvinning av substringer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

### Hva & Hvorfor?
Å ekstrahere substringer, eller delstrenger på norsk, er rett og slett å hente ut en del av en lengre tekststreng. Dette kan være nyttig for å bearbeide data eller for å finne spesifikke mønstre i en tekst. Programmører gjør dette for å organisere og manipulere tekstbaserte data på en effektiv måte.

### Hvordan:
```Rust
let tekst = "Hei, dette er en tekststreng!";
let delstreng = &tekst[8..];
println!("Delstrengen er: {}", delstreng); // output: "er en tekststreng!"
```

```Rust
let tall = "8675309";
let først_tall = &tall[..4]; // henter de første 4 sifrene
println!("Første tall er: {}", først_tall); // output: "8675"
```

### Deep Dive:
I tidligere versjoner av Rust var det ikke mulig å ekstrahere substringer. Dette førte til at man måtte kopiere og manipulere teksten manuelt, noe som kunne være tungvint og ineffektivt. Nå kan man enkelt bruke slicing syntax for å hente ut delstrenger. Andre måter å hente ut deler av en tekststreng inkluderer å bruke regex-uttrykk eller bruk av biblioteker som hjelper med å utføre mer komplekse operasjoner på tekst.

### Se også:
- [Rust slicing documentation](https://doc.rust-lang.org/std/primitive.slice.html)
- [Regex dokumentasjon for Rust](https://docs.rs/regex/1.5.4/regex/)
- [Rust Text-Processing Libraries](https://www.rust-lang.org/learn/ecosystem#text-processing)