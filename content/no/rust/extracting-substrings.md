---
title:    "Rust: Uttrekking av delstrenger"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Har du noen gang ønsket å separere en del av en tekststreng i Rust? Kanskje du trenger å hente ut et bestemt ord eller en frase fra en større tekst, eller kanskje du vil gjøre noen manipulasjoner på en del av en URL. Uansett hva grunnen er, er det enkelt å gjøre med Rusts innebygde funksjoner for å utvinne substrings.

# Slik gjør du det

Det finnes flere måter å få tilgang til substrings i Rust, avhengig av hva du trenger å gjøre med dem. Her er noen eksempler for å vise deg hvordan du kan utvinne substrings på forskjellige måter.

```Rust
// Opprett en tekststreng
let teksten = "Dette er en tekststreng å øve seg på";

// Hent ut et ord fra teksten
let ord = &teksten[13..15];
println!("{}", ord);

// Output: en

// Hent ut en frase fra teksten
let frase = &teksten[17..30];
println!("{}", frase);

// Output: tekststreng å

// Hent ut en del av en URL
let url = "https://www.example.com/blog/rust";
let del = &url[27..];
println!("{}", del);

// Output: rust
```

Som du kan se, bruker vi firkantede parenteser og et område av tall for å angi hvor i tekststrengen vi ønsker å få tilgang til. Det første tallet angir startpunktet, mens det andre tallet angir sluttpunktet (ikke inkludert).

# Dype dykk

Det er verdt å nevne at når du utvinner substrings, vil den faktiske datastrukturen være det samme as originalteksten. Dette betyr at substrings er "lånt" fra originalteksten og at endringer i substringsen vil reflekteres tilbake til originalteksten. Det er derfor viktig å være forsiktig når du endrer substrings, og eventuelt heller opprette en ny tekststreng basert på substringen.

Det er også viktig å merke seg at substrings i Rust er en del av Unicode-støtten, noe som betyr at de kan håndtere ikke-ASCII-tegn som f.eks. æ, ø, å og andre tegn fra ulike språk. Dette gjør at Rust er et flott språk for å håndtere tekstmanipulasjon på et internasjonalt nivå.

# Se også
- [Rust dokumentasjon om tekstmanipulasjon](https://doc.rust-lang.org/std/primitive.str.html#methods)
- [Tutorial om hvordan å arbeide med tekst i Rust](https://dev.to/damcosset/working-with-text-in-rust-1l4g)