---
title:    "Rust: Oversette en dato til et strengformat"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Hvorfor
Har du noen gang ønsket å konvertere en dato til en streng, for eksempel for å vise den på en nettside eller i en tekstfil? Da kan du være interessert i å lære hvordan du gjør dette i Rust. Rust er et relativt nytt programmeringsspråk som har blitt stadig mer populært på grunn av sin ytelse og sikkerhet.

# Hvordan Du Gjør Det
Konvertering av en dato til en streng i Rust kan gjøres ved hjelp av standardbiblioteket, spesifikt ved hjelp av "format" metoden fra "DateTime" strukturen. La oss se på et eksempel:

```Rust
use std::time::SystemTime;
use chrono::prelude::*; //importerer chrono bibloteket for manipulering av datoer og tider

fn main() {
    let now = SystemTime::now(); //henter nåværende tidspunkt
    let date: DateTime<Utc> = now.into(); //konverterer nåværende tidspunkt til en DateTime struktur
   
    let date_string = date.format("%Y-%m-%d").to_string(); //bruker format metoden for å konvertere datoen til ønsket strengformat
    println!("Dagens dato er {}", date_string); //printer ut datoen i ønsket format
}
```

Output:
```
Dagens dato er 2021-05-06
```

I dette eksempelet bruker vi "format" metoden til å konvertere datoen til en string med formatet "ÅÅÅÅ-MM-DD". Du kan også bruke andre formatstrukturer, avhengig av hva som passer dine behov.

# Dypdykk
La oss se nærmere på hvordan "format" metoden fungerer. Den tar to argumenter: en string som indikerer det ønskede formatet, og en "DateTime" struktur. Formatstrengen består av ulike formateringstegn som angir hvordan datoen skal vises.

For eksempel representerer "%Y" det fire-sifrede året, "%m" representerer månedsnummeret, og "%d" representerer dagens nummer. Du kan finne en full liste over formateringstegn på Rusts dokumentasjonsside.

Det er også mulig å kombinere flere formateringstegn, for eksempel "%Y-%m-%d" som vi brukte i eksempelet ovenfor. Dette vil resultere i en string med formatet "ÅÅÅÅ-MM-DD".

# Se Også
- [Rust Dokumentasjon](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Chrono Bibloteket for Manipulering av Datoer og Tider](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Formatering av Datoer i Rust](https://rust-lang-nursery.github.io/rust-cookbook/datetime/formatting.html)