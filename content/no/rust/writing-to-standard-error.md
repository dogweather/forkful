---
title:                "Skriving til standardfeilen"
html_title:           "Rust: Skriving til standardfeilen"
simple_title:         "Skriving til standardfeilen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriver til standardfeil er et begrep som brukes av programmerere for å beskrive handlingen med å skrive ut feilmeldinger og andre viktige meldinger til standard error-strømmen. Dette er en måte å synliggjøre feil og annen kritisk informasjon i koden din mens programmet kjører.

## Slik går du frem:
For å skrive en melding til standardfeil i Rust, bruker du ```eprintln!()``` makroen. Dette vil skrive ut meldingen du angir etterfulgt av en ny linje. Her er et eksempel:

```Rust
eprintln!("Noe gikk galt!");
```
Dette vil skrive ut "Noe gikk galt!" til standardfeil-strømmen.

## Dypdykk:
Historisk sett var standardfeil bare brukt til å håndtere feilmeldinger, mens standardutdata ble brukt til å skrive ut vanlig informasjon. Men i dag brukes det vanligvis til å skrive ut all slags informasjon som ikke skal vises for brukeren.

En alternativ måte å skrive til standardfeil i Rust er å bruke ```write!()``` makroen. Dette vil skrive ut en melding til et spesifikt sted, som standardfeil. 

Implementeringen av standardfeil-funksjonen i Rust er basert på standard feilhåndtering i Unix og C, som bruker en numerisk kode for å indikere forskjellige typer feil.

## Se også:
- [Rust Dokumentasjon](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Rust Standard Library](https://doc.rust-lang.org/std/io/struct.Stderr.html)