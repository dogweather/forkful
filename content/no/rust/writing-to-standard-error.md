---
title:                "Rust: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skrive til standardfeil i Rust-programmering? Det kan være flere grunner til dette, men ofte er det for å gi mer informasjon om hva som skjer i programmet ditt mens det kjører. Dette kan være nyttig for debugging eller for å gi brukeren en bedre forståelse av programmet.

## Hvordan gjøre det

Det er enkelt å skrive til standardfeil i Rust-programmering. Du kan bruke funksjonen `eprintln!` som tar inn en formatstreng og eventuelle verdier du vil inkludere. La oss se på et eksempel:

```Rust
eprintln!("Feil: {} er ikke et gyldig tall", num);
```

I dette tilfellet vil verdien av `num` bli satt inn i formatstrengen, og teksten vil bli skrevet til standardfeil. Output vil se slik ut:

```
Feil: 10 er ikke et gyldig tall
```

Hvis du vil skrive til standardfeil uten å bruke en formatstreng, kan du bruke `writeln!`-funksjonen. Denne funksjonen fungerer på samme måte som `eprintln!`, men legger også til en linjeskift på slutten.

Det er også mulig å bruke `eprint!` og `print!` for å skrive til standardfeil og standardutgang, henholdsvis.

## Dypdykk

Når du skriver til standardfeil i Rust, vil du kanskje også vite hva som skjer i kulissene. Når du bruker `eprintln!` eller `eprint!`, blir standardfeilstrømmen brukt. Dette betyr at utskriften ikke vil bli påvirket av utdaterte operativsystemnivåbuffere og vil bli skrevet umiddelbart.

Det kan også være nyttig å vite at det er mulig å endre standardfeilstrømmen ved å bruke funksjonen `set_panic_hook`. Dette gjør det mulig å skrive ut feilmeldinger eller tracebacks til hvilken som helst strøm som du ønsker.

## Se også

- [Rust dokumentasjon om standardfeil](https://doc.rust-lang.org/std/io/struct.Stderr.html)
- [Rust dokumentasjon om strømmer](https://doc.rust-lang.org/std/io/trait.Write.html)
- [Artikkel om standardfeil i Rust](https://andrewkinnear.tech/2019/08/29/understanding-the-println-macro-and-standard-error-in-rust/)