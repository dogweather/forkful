---
title:    "Rust: Stor bokstavering av en streng"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan endre et strengens første bokstav til en stor bokstav i Rust? Eller kanskje du allerede vet hvordan, men vil lære en mer effektiv måte å gjøre det på? Uansett hva som er grunnen din, er dette blogginnlegget for deg! Vi skal dykke inn i hvordan du kan kapitalisere en streng i Rust på en enkel og effektiv måte.

## Hvordan Gjøre Det

Det er flere måter å kapitalisere en streng på i Rust, men la oss se på den mest brukte og anbefalte måten å gjøre det på. Her er et eksempel på kode som viser hvordan du kan gjøre det:

```Rust
let name = "ola";
let capitalized_name = name.to_uppercase();
println!("Kapitalisert navn: {}", capitalized_name);

```

Når du kjører denne koden, vil den gi følgende utdata:

```
Kapitalisert navn: OLA
```

Her bruker vi funksjonen `to_uppercase()` som er en del av `String` biblioteket i Rust. Denne funksjonen returnerer en ny streng med den første bokstaven som stor bokstav. Deretter kan vi bare skrive ut den nye strengen ved hjelp av `println!` makroen.

Videre kan du også kapitalisere bare den første bokstaven i en streng ved å bruke `capitalize()` funksjonen. Her er et eksempel på hvordan du kan gjøre det:

```Rust
let greeting = "hei der";
let capitalized_greeting = greeting.to_lowercase().capitalize();
println!("Kapitalisert hilsen: {}", capitalized_greeting);

```

Dette vil gi følgende utdata:

```
Kapitalisert hilsen: Hei der
```

I tillegg kan du også sjekke om en streng allerede er kapitalisert eller ikke ved å bruke `is_ascii_uppercase()` funksjonen. Denne funksjonen vil returnere en boolsk verdi som indikerer om strengen bare har store bokstaver eller ikke.

## Dykk Ned

Som nevnt tidligere, er `to_uppercase()` funksjonen en del av `String` biblioteket i Rust. Dette biblioteket tilbyr forskjellige metoder for å håndtere og manipulere strenger på en enkel og effektiv måte. Ved å lese gjennom dokumentasjonen og eksperimentere med forskjellige funksjoner, kan du få dypere forståelse for hvordan du kan kapitalisere en streng på en mer avansert måte.

## Se Også

- Rust Språkvedlikehold (https://www.rust-lang.org/no)
- Rust Dokumentasjon (https://doc.rust-lang.org/std/string/struct.String.html)
- Utforsk Rust (https://www.rust-lang.org/learn)