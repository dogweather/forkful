---
title:                "Rust: Stor bokstavering av en streng"
simple_title:         "Stor bokstavering av en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Rust er et nytt og spennende programmeringsspråk som blir stadig mer populært blant utviklere. En av de mange fordelene med Rust er dens evne til å håndtere strenger på en effektiv og sikker måte. I denne bloggposten skal vi se nærmere på hvordan man kan gjøre strenger mer leselige ved å kapitalisere dem.

## Hvordan

For å kapitalisere en streng i Rust, trenger vi bare å bruke funksjonen `to_uppercase()` sammen med strengvariabelen vår. La oss se på et enkelt eksempel:

```Rust
let navn = "ole";
println!("{}", navn.to_uppercase());
```

Dette vil gi følgende output:

```
OLE
```

Vi kan også kapitalisere en streng basert på språk og regionale konvensjoner ved å bruke funksjonen `to_uppercase_with_locale()`. For eksempel:

```Rust
let setning = "jeg elsker rust";
println!("{}", setning.to_uppercase_with_locale("nb_NO"));
```

Dette vil gi følgende output:

```
JEG ELSKER RUST
```

## Deep Dive

Når vi kapitaliserer en streng i Rust, blir det egentlig laget en helt ny streng med de samme tegnene i store bokstaver. Dette skjer fordi Rust-strukturer er uforanderlige, noe som betyr at vi ikke kan endre på en allerede opprettet streng. Derfor kan det å kapitalisere en streng bli en kostbar operasjon, spesielt hvis strengen er veldig lang.

En annen viktig ting å merke seg er at kapitaliseringen kun fungerer for alfanumeriske tegn og ikke for spesialtegn eller tall.

## Se Også

- [Rust dokumentasjon for strenger](https://doc.rust-lang.org/std/string/)
- [Mer om Rust-programmering på norsk](https://github.com/AXelqvist/rust-programming-language-blog)