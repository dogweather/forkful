---
title:                "Rust: Skriving til standardfeil"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor skrive til standard error i Rust?

Å skrive til standard error er en viktig måte å håndtere feil og debugging i Rust-programmering. Når ting går galt, er det nyttig å kunne sende ut spesifikke feilmeldinger til standard error for å hjelpe med å identifisere og løse problemer.

# Hvordan gjøre det i praksis

Først må vi importere `std::io` modulen for å kunne skrive til standard error. Her er et eksempel på en enkel funksjon som skriver en feilmelding til standard error:

```Rust
use std::io;

fn main() {
    println!("Dette er en test av å skrive til standard error!");

    let err = "Dette er en feilmelding."; // En variabel for vår feilmelding
    let stderr = &mut io::stderr(); // Oppretter en mutable reference til standard error
    writeln!(stderr, "{}", err).expect("Kunne ikke skrive til standard error.");
}
```

La oss si at vi ønsker å inkludere linjenummeret i vår feilmelding slik at det blir enklere å spore opp hvor i programmet feilen oppstod. Da kan vi benytte oss av `line!` makroen, som returnerer et tall for nåværende linje i koden:

```Rust
use std::io;

fn main() {
    let linje = line!(); // Henter nåværende linjenummer
    let err = format!("En feil oppstod på linje {}.", linje); // Oppretter feilmelding med linjenummeret
    let stderr = &mut io::stderr(); // Oppretter en mutable reference til standard error
    writeln!(stderr, "{}", err).expect("Kunne ikke skrive til standard error.");
}
```

Dette vil gi oss en feilmelding som inneholder linjenummeret, noe som kan være veldig nyttig når man jobber med større og mer komplekse koder.

# Dypdykk i å skrive til standard error

Et annet nyttig aspekt ved å skrive til standard error i Rust er muligheten til å benytte seg av `Error` traitet. Dette lar oss lage egendefinerte feilobjekter som kan gi mer nyttig informasjon om feil som oppstår.

La oss for eksempel si at vi ønsker å lage et bildebibliotek hvor vi ønsker å ha en egen feiltype for når et bilde ikke kan lastes inn. Vi kan da definere vår egen `BildeError` som implementerer `Error` traitet:

```Rust
use std::error::Error;
use std::fmt;

pub struct BildeError {
    pub beskrivelse: String,
}

// Vi implementerer fmt::Display og fmt::Debug for å kunne skrive ut feilmeldingen
impl fmt::Display for BildeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Kunne ikke laste inn bilde: {}", self.beskrivelse)
    }
}

impl fmt::Debug for BildeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Kunne ikke laste inn bilde: {}", self.beskrivelse)
    }
}

// Og implementerer Error traitet for å få tilgang til metoder som for eksempel backtrace()
impl Error for BildeError {}
```

Vi kan nå bruke vårt egendefinerte `BildeError` for å håndtere feil ved innlasting av bilder på en mer strukturert og informativ måte.

# Se også

- [Rust dokumentasjon om io::stderr()](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [Rust dokumentasjon om line!() makroen](https://doc.rust-lang.org/std/macros/macro.line.html)
- [Rust dokumentasjon om Error traitet](https://doc.rust-lang.org/std/error/trait.Error.html)