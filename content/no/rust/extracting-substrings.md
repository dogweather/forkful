---
title:                "Rust: Uttrekk av delstrenger"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang hatt behov for å hente ut deler av en tekststreng i et programmeringsprosjekt? Kanskje du trenger å filtrere ut bestemte ord eller bokstaver fra en lengre tekst? Da vil denne bloggposten om å ekstrahere delstrenger i Rust være noe for deg!

## Hvordan

Ekstrahering av delstrenger er en grunnleggende funksjon i mange programmeringsspråk, og Rust er inget unntak. La oss se på et enkelt eksempel på hvordan du kan bruke dette i ditt eget program.

```Rust
let text = "Hei, dette er en tekststreng.";

// Ekstraher en delstreng fra og med indeks 4 til og med indeks 14
let substring = &text[4..15];

println!("Utdrag av originalteksten: {}", substring);
```

Dette vil skrive ut "dette er en" i konsollen, som er delen av tekststrengen som vi ekstraherte. En ting som er viktig å merke seg er at Rusts substring-ekstraksjon er null-indeksert, det vil si at det første tegnet i en tekststreng har indeks 0.

En annen måte å ekstrahere delstrenger på, er å bruke metoden `substring()` sammen med `find()`, som gir deg muligheten til å finne en spesifikk del av en tekst. Her er et eksempel på dette:

```Rust
let text = "Enkelt eksempel på ekstraksjon av en delstreng.";
let keyword = "ekstraksjon";

if let Some(index) = text.find(keyword) {
    let substring = text.substring(index, index + keyword.len());

    println!("Tekststrengen inneholder ordet \"{}\"", substring);
} else {
    println!("Ordet \"{}\" finnes ikke i tekststrengen.", keyword);
}
```

Dette vil skrive ut "ordet "ekstraksjon" i konsollen. Her bruker vi metoden `find()` til å finne indeksen til det første tegnet i ordet "ekstraksjon", og deretter bruker vi `substring()` til å ekstrahere ordet og skrive det ut.

## Dypdykk

I Rust er delstrenger representert som en `&str` type, som er en referanse til den originale tekststrengen. Dette betyr at delstrengen deler den samme dataen som den originale tekststrengen, noe som gjør ekstraksjon av delstrenger veldig effektivt og minnevennlig.

I tillegg til de nevnte metodene, finnes det også andre måter å ekstrahere delstrenger på i Rust. For eksempel kan du også bruke `chars()` og `split()` metoder for mer komplekse ekstraksjonsoperasjoner.

## Se også

- [Rust dokumentasjon om delstreg-ekstraksjon](https://doc.rust-lang.org/std/primitive.str.html#method.substring)
- [Rust-by-example om delstreng-ekstraksjon](https://doc.rust-lang.org/stable/rust-by-example/std/str.html#substring)
- [Offisiell Rust-nettside](https://www.rust-lang.org/)