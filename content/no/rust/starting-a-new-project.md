---
title:    "Rust: Å starte et nytt prosjekt"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor
Rust er et kraftig programmeringsspråk som er kjent for sin stabilitet, sikkerhet og gode ytelse. Det er et ideelt valg for å utvikle pålitelige og effektive applikasjoner. Hvis du ønsker å starte et nytt programmeringsprosjekt, bør du definitivt vurdere å bruke Rust.

## Hvordan
La oss se på et enkelt eksempel på hvordan du kan bruke Rust til å lage en funksjon som legger sammen to tall og returnerer summen:

```Rust
fn legg_til(tall1: i32, tall2: i32) -> i32 {
    return tall1 + tall2;
}

// Kjør funksjonen med tallene 5 og 10
let sum = legg_til(5, 10);

println!("Summen av 5 og 10 er {}", sum);
```

Dette vil produsere følgende output:

```
Summen av 5 og 10 er 15
```

Som du kan se, er syntaksen i Rust ganske enkel og lesbar. Du definerer funksjoner med nøkkelordet `fn`, og angir hvilke parametere som skal tas i mot. Du må også spesifisere hvilken type verdi som skal returneres.

## Dypdykk
Når du starter et nytt prosjekt i Rust, er det viktig å sette opp riktig miljø og verktøy. Det er flere ulike IDE-er som støtter Rust-utvikling, som for eksempel Visual Studio Code, IntelliJ og Atom. Du kan også installere Rust og Rust-verktøyene direkte på datamaskinen din. Det kan også være lurt å lære om konseptet med Rusts packagemanager, cargo, og hvordan man bruker den til å håndtere avhengigheter og bygge prosjektet ditt.

Når du har etablert et godt grunnlag, kan du begynne å utforske ulike biblioteker og rammerverk som finnes for Rust. Disse kan hjelpe deg med å bygge mer komplekse og funksjonsrike applikasjoner. Fordi Rust er et relativt nytt språk, kan det være lurt å søke etter dokumentasjon og eksempler for å hjelpe deg i gang.

## Se også
- [Rust offisiell nettside](https://www.rust-lang.org/no)
- [Rust dokumentasjon](https://doc.rust-lang.org/book/)
- [Rust pakkehåndterer (cargo)](https://doc.rust-lang.org/cargo/)
- [Minimal Rust IDE (rust-analyzer)](https://rust-analyzer.github.io/)