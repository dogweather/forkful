---
title:                "Rust: Ekstrahere substrings"
simple_title:         "Ekstrahere substrings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å utvinne substrings? Det er flere grunner til å gjøre dette i Rust. En av de vanligste grunnene er å manipulere tekststrenger mer effektivt og enkelt. Ved å utvinne substrings kan du få tilgang til og endre deler av en tekststreng uten å måtte kopiere hele strengen.

## Hvordan

For å utvinne substrings i Rust, kan du bruke "slice" -operatøren ([..]) eller metoden .split_at (). Her er et eksempel på å bruke slice-operatøren:

```rust
let original = "Dette er en tekststreng";
let substring = &original[6..13]; // "er en t"
```

Metoden .split_at () gjør det mulig å utvinne en tekststreng på et bestemt indeks:

```rust
let original = "Dette er en tekststreng";
let (start, end) = original.split_at(9); // start = "Dette er ", end = "en tekststreng"
```

## Deep Dive

Når du utvinner en substring i Rust, blir det opprettet en ny tekststreng som deler den samme minneplassen som den opprinnelige strengen. Dette betyr at å utvinne substringer er en effektiv måte å håndtere tekst på, da det ikke krever mer minne.

En annen nyttig funksjon i Rust er .get () -metoden, som gjør det mulig å hente en enkelt karakter fra en tekststreng basert på indeksen:

```rust
let original = "Dette er en tekststreng";
let character = original.get(5); // character = Some('e')
```

Hvis indeksen overskrider lengden på tekststrengen, vil metoden returnere None. Dette gjør det også mulig å unngå feil som kan oppstå ved å bruke indeksering utenfor grensene til en streng.

## Se også

- Rust dokumentasjon om tekstmanipulering: https://doc.rust-lang.org/std/string/
- Alternativer for tekstbehandlingsbiblioteker i Rust: https://github.com/brson/stdx/issues/2
- Eksempelkode for å utvinne substringer i Rust: https://github.com/mkpankov/trade-server/blob/master/src/main.rs