---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Rust: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert i Rust, har du sannsynligvis også kommet over behovet for å skrive ut feilsøkingsinformasjon. Selv om Rust er kjent for sin strenge typetilordning og kompilatorfeil, kan det fortsatt oppstå situasjoner der du trenger å se hva som foregår i koden din for å finne feilen. Heldigvis tilbyr Rust en enkel og kraftig metode for å skrive ut debug-utdata.

## Hvordan

For å skrive ut debug-utdata i Rust, kan du bruke makroen `println!`. Denne makroen fungerer på samme måte som `println!` i andre programmeringsspråk, bortsett fra at den krever et ekstra symbol `#` før anførselstegnet for å skrive ut feilsøkingsinformasjon. Her er et eksempel:

```Rust
fn main() {
    let navn = "Kari";
    println!("Navn: {:?}", navn); // Legg merke til # symbolet

    let nummer = 123;
    println!("Nummer: {:?}", nummer);
}
```

Dette vil resultere i følgende utskrift:

```
Navn: "Kari"
Nummer: 123
```

Som du kan se, skriver `println!` ut verdien til variabelen, i dette tilfellet en tekststreng og et heltall. Ved å bruke `{:?}`, forteller du Rust at du vil skrive ut verdien på en måte som er nyttig for feilsøking. Hvis du vil skrive ut verdien på en mer vanlig måte, kan du bruke `{}` i stedet.

Du kan også bruke `print!` makroen hvis du ikke ønsker å inkludere en ny linje i utskriften. Dette kan være nyttig hvis du vil skrive ut flere linjer med informasjon i samme funksjon.

## Deep Dive

Når du bruker `println!`, kan du skrive ut så mange variabler som du vil, ved å inkludere dem i kommandoen, for eksempel `println!("Verdi 1: {}, Verdi 2: {}", verdi1, verdi2)`. I tillegg kan du også bruke spesielle formateringsmetoder for å gjøre utskriften mer lesbar, for eksempel å begrense desimaler på et tall eller skrive ut binærkoden for en verdi.

En annen nyttig funksjon for debug-utdata er makroen `dbg!`, som skriver ut både navnet på variabelen og verdien den inneholder. Her er et eksempel:

```Rust
fn main() {
    let navn = "Kari";
    dbg!(navn); // Legg merke til at du ikke trenger å bruke # symbolet her

    let nummer = 123;
    dbg!(nummer);
}
```

Dette vil resultere i følgende utskrift:

```
[src/main.rs:4] navn = "Kari"
[src/main.rs:7] nummer = 123
```

Som du kan se, kan `dbg!` være spesielt nyttig når du jobber med flere variabler og trenger å holde styr på hvilken verdi som tilhører hvilken variabel.

## Se også

- [Rust dokumentasjon om debugging](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html#reading-the-environment-to-configure-your-program)
- [Artikkel om Rusts debugging-funksjoner](https://blog.burntsushi.net/rust-debugging/)