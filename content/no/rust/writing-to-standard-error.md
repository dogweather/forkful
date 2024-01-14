---
title:    "Rust: Å skrive til standardfeil"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Hvorfor skrive til standard error i Rust?
Å skrive til standard error i Rust er en enkel og effektiv måte å håndtere feil og debugging på. Det lar deg raskt og nøyaktig identifisere og håndtere eventuelle problemer i koden din.

## Hvordan gjøre det
For å skrive til standard error i Rust, kan du bruke funksjonen `eprintln!()` eller `writeln!()`. Disse funksjonene lar deg skrive en feilmelding eller annen informasjon til standard error stream, som kan leses fra terminalen.

```Rust
fn main() {
    let x = 10;
    let y = 0;
    if y == 0 {
        eprintln!("Kan ikke dele på 0!");
        return;
    }
    let result = x / y;
    println!("Resultat: {}", result);
}
```

Output:
```
Kan ikke dele på 0!
```

I dette eksempelet har vi brukt `eprintln!()` for å skrive en feilmelding når forsøket på å dele på 0 feiler.

## Dypdykk
Når du skriver til standard error i Rust, kan du også bruke *macros* som `panic!()` for å håndtere alvorlige feil og avslutte programmet. Du kan også bruke `eprint!()` og `print!()` for å skrive til standard error og standard output henholdsvis.

Det er viktig å merke seg at skriving til standard error vil føre til en linjeskift etter hvert kall til `eprintln!()`. Dette kan være nyttig for å skille ut- og inn-data i konsollen når du kjører programmet ditt.

# Se også
- [Offisiell Rust dokumentasjon om skriving til standard error](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Tutorial om debugging i Rust](https://www.rust-lang.org/tools/debugging)
- [Stack Overflow diskusjon om forskjellen mellom `print!()` og `eprint!()`](https://stackoverflow.com/questions/34693801/what-exactly-is-the-difference-between-print-and-eprint-macros)