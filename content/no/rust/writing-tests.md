---
title:                "Rust: Skrevetester"
simple_title:         "Skrevetester"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive tester i programmering er en viktig og nødvendig del av utviklingsprosessen. Tester bidrar til å sikre at koden fungerer som den skal og reduserer risikoen for feil og bugs i produksjon. Det kan også bidra til å forbedre kvaliteten og holdbarheten til kodebasen din.

# Hvordan

For å skrive tester i Rust, må du først importere "test" biblioteket ved å legge til følgende linje øverst i filen din:

```Rust
#[test]
```

Deretter kan du bruke tester ved å legge til egendefinerte funksjoner og bruke assert-makroen for å sjekke om ønsket resultat er oppnådd. Her er et eksempel på en enkel test av en "add" funksjon:

```Rust
#[test]
fn test_add() {
    let result = add(2, 3); // Kaller funksjonen "add" med to tall og lagrer resultatet i en variabel
    assert_eq!(result, 5); // Sjekker om resultatet er lik forventet svar
}
```

Når du kjører denne testen, vil du få en melding om at testen er bestått eller mislyktes, samt informasjon om hvilken linje som eventuelt feilet.

# Dypdykk

Det finnes flere typer tester du kan skrive i Rust, blant annet unit tester, integrasjonstester og ytelsestester. Det er også mulig å bruke "test" biblioteket til å generere dokumentasjon ved hjelp av kodeeksempler.

En viktig regel å følge når man skriver tester i Rust er at hver testfunksjon skal være uavhengig og ikke påvirke andre tester. Dette sikrer at resultatene er pålitelige og at eventuelle feil kan spores tilbake til en spesifikk test.

Hvis du ønsker å lære mer om å skrive tester i Rust, kan du sjekke ut dokumentasjonen for "test" biblioteket på Rusts offisielle nettside.

# Se også

* [Rust dokumentasjon - test biblioteket](https://doc.rust-lang.org/std/macro.assert.html)
* [En innføring i å skrive tester i Rust](https://adventures.mechanicalrock.io/rust-introduction-w-writing-unit-tests.html)
* [Hvordan skrive gode tester i Rust](https://medium.com/@adiswami/a-concise-guide-to-writing-tests-in-rust-83116dd9a344)