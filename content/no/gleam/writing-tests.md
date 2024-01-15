---
title:                "Å skrive tester"
html_title:           "Gleam: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive tester kan virke som en ekstra byrde når du allerede har mye kode å skrive. Men å inkludere tester i koden din kan spare deg for mye tid og frustrasjon på lang sikt. Tester hjelper deg med å finne feil tidlig i utviklingsprosessen, noe som gjør det enklere og raskere å rette dem. De bidrar også til å sikre at koden din fortsetter å fungere som forventet når du gjør endringer eller legger til ny funksjonalitet.

# Hvordan

For å skrive tester i Gleam, bruker vi rammeverket Gleam Expect. Dette lar oss definere forventet oppførsel for funksjonene våre, og kjøre tester for å sørge for at de faktisk oppfører seg som de skal. La oss se på et eksempel:

```
Gleam Expect.suite "Tester for addisjonsfunksjon"
  "Skal returnere summen av to tall" {
    Gleam Expect.equal (Calculator.addition 2 3) 5
  }
```
Dette kodeeksempelet definerer en test-suite med en enkel test. Vi forventer at funksjonen `addition` fra en hypotetisk `Calculator`-modul skal returnere summen av to tall, og sjekker at dette faktisk skjer ved å bruke `Gleam Expect.equal`.

For å kjøre testene våre, kan vi kjøre følgende kommando i terminalen:

```
gleam test
```

Dette vil deretter vise resultatene av testene våre, og gi oss beskjed om noen feiler.

# Dypdykk

I Gleam kan vi også skrive tester på typer for å sikre at de oppfører seg som forventet. Dette kan være spesielt nyttig når vi jobber med komplekse datastrukturer eller moduler som er avhengige av bestemte typer.

For eksempel kan vi definere en test for å sjekke at en liste kun inneholder heltall:

```
Gleam Expect.suite "Tester for liste-typer"
  "Skal bare tillate heltall i en liste" {
    let valid_list = [1,2,3]
    let invalid_list = ["a", 1, 2]

    Gleam Expect.all [
      Gleam Expect.type_valid Int valid_list
      Gleam Expect.type_valid Int invalid_list
    ]
  }
```

Her bruker vi `Gleam Expect.type_valid` for å sjekke at typen til elementene i en liste er som forventet. Dette kan være nyttig når vi ønsker å sikre at funksjoner kun tar inn bestemte typer, eller når vi ønsker å unngå utilsiktede feil i kodesnuttene våre.

# Se også

- [Gleam dokumentasjon for testing](https://gleam.run/documentation/unit_testing)
- [Gleam Expect dokumentasjon](https://gleam.run/documentation/expect)
- [Eksempelprosjekt med Gleam tester](https://github.com/gleam-lang/gleam-test-example)