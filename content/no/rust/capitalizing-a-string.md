---
title:                "Rust: Å sette store bokstaver på en tekststreng"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng (string) kan være nødvendig når du ønsker å formatere teksten din på en ryddig og enhetlig måte. Det kan også være nyttig når du ønsker å skille mellom store og små bokstaver for å gjøre søk og sortering enklere. Uansett årsak, kan Rust-programmerere dra nytte av å vite hvordan man gjør dette.

## Hvordan gjøre det

For å kapitalisere en streng i Rust, kan du bruke den innebygde metoden "to_uppercase()". La oss se på et eksempel:

```Rust
let name = "per";
println!("Original streng: {}", name);
let capitalized_name = name.to_uppercase();
println!("Kapitalisert streng: {}", capitalized_name);
```

Dette vil gi følgende utgang:

```
Original streng: per
Kapitalisert streng: PER
```

I dette tilfellet ble strengen "per" endret til store bokstaver.

## Dypdykk

I tillegg til "to_uppercase()" metoden, kan du også bruke "to_lowercase()" for å gjøre en streng til små bokstaver. Hvis du ønsker å endre bare den første bokstaven i en streng til stor bokstav, kan du bruke "to_uppercase()"-metoden på bare den første bokstaven. Det finnes også andre metoder i Rust som kan endre en streng, som for eksempel "capitalize()", som bare endrer den første bokstaven, og "make_ascii_uppercase()", som endrer alle bokstavene til store ASCII-bokstaver.

## Se også

- Dokumentasjon for strengmetoder i Rust: https://doc.rust-lang.org/std/string/
- En guide til å formatere strenger i Rust: https://www.educative.io/edpresso/how-to-capitalize-strings-in-rust
- En tutorial om strenger i Rust: https://www.youtube.com/watch?v=3fUbBnN_H1c