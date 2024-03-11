---
date: 2024-01-26 00:57:14.324645-07:00
description: "Feilh\xE5ndtering handler om \xE5 takle ting n\xE5r de skj\xE6rer seg.\
  \ Programmerere gj\xF8r dette for \xE5 h\xE5ndtere det uventede, slik at deres Rust-programmer\
  \ er robuste\u2026"
lastmod: '2024-03-11T00:14:14.117636-06:00'
model: gpt-4-1106-preview
summary: "Feilh\xE5ndtering handler om \xE5 takle ting n\xE5r de skj\xE6rer seg. Programmerere\
  \ gj\xF8r dette for \xE5 h\xE5ndtere det uventede, slik at deres Rust-programmer\
  \ er robuste\u2026"
title: "Feilh\xE5ndtering"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Feilhåndtering handler om å takle ting når de skjærer seg. Programmerere gjør dette for å håndtere det uventede, slik at deres Rust-programmer er robuste og ikke bare krasjer ved en hikke.

## Hvordan:

Rust håndterer feil på to hovedmåter: gjenvinnbare og ugjenkallelige feil. La oss se på begge.

For gjenvinnbare feil bruker vi `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Filen ble åpnet vellykket."),
        Err(_e) => println!("Kunne ikke åpne filen."),
    }
}
```

Utdata kan være enten "Filen ble åpnet vellykket." eller "Kunne ikke åpne filen." avhengig av din `hello.txt`.

For ugjenkallelige feil bruker vi `panic!`:

```Rust
fn main() {
    // Dette vil forårsake at programmet panikker fordi filen sannsynligvis ikke eksisterer.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Kjør det, og du vil se en panikkmelding. Programmet ditt stopper dødt i sporene.

## Dypdykk

Historisk sett har feilhåndtering i programmering vært et rot. Rust får det riktig med en klar forskjell mellom gjenvinnbare og ugjenkallelige feil.

`Result`-enumen er for gjenvinnbare feil. Det er eksplisitt – du håndterer `Ok`- eller `Err`-varianten. Du har metoder som `unwrap()` og `expect()` også, men de er kjappe og skitne snarveier som kan føre til `panic!`.

`panic!` er Rusts måte å rope ut at noe virkelig ille har skjedd, og den kan ikke takle det. Det er som en ugjenkallelig feil som stopper utførelsen umiddelbart. Et panikk i Rust registreres ofte med feil du ikke forventer å håndtere, som å indeksere utenfor arraygrenser.

Feilhåndtering ved å returnere `Result` foretrekkes når du forventer å håndtere feil. Det er idiomer i Rust, som betyr at det er måten Rust-utviklere har blitt enige om å gjøre ting på. Det finnes også `Option<T>`, for tilfeller der en feil bare er noe som er `None` i stedet for `Some(T)`. Det handler alt om å forvente det uventede uten frykt.

Alternativer? Sikkert, du kunne bruke andre feilhåndteringsbiblioteker for flere funksjonaliteter eller ergonomisk bruk. Som `anyhow` for enkel feilhåndtering, eller `thiserror` for feil i bibliotekskode.

## Se Også

Interessert i å dykke dypere? Her er hvor du kan gå:

- [Rust Book on Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Et flott sted for å forstå Rusts filosofi for feilhåndtering.
- [Rust by Example: Error handling](https://doc.rust-lang.org/rust-by-example/error.html) - Interaktive eksempler for å få hendene skitne.

Husk, god feilhåndtering er ikke bare koding; det er å ta vare på brukerne av koden din. God koding!
