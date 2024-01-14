---
title:                "Gleam: Skriving til standardfeil"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skrive til standard error? 

Noen ganger når man koder, kan det være nyttig å sende en melding til standard error (oftest forkortet til stderr). Dette er en vanlig måte å kommunisere feil og varselmeldinger til brukeren. Ved å skrive til stderr kan man få et bedre innblikk i eventuelle problemer i koden og feilsøke på en enklere måte. 

# Hvordan

Hvordan skrive til standard error i Gleam? 

For å skrive til stderr i Gleam kan man bruke funksjonen `io.write_err()` som tar inn en streng som parameter. Her er et eksempel på hvordan dette kan gjøres: 

```Gleam
fn main() {
    io.write_err("Dette er en melding til standard error")
}
```

Dette vil skrive ut meldingen "Dette er en melding til standard error" til stderr. Dersom man ønsker å inkludere variabler i meldingen, kan man bruke formateringsstrenger slik som `%s` eller `%i`, og deretter oppgi variablene som tilhørende parametere. Her er et eksempel: 

```Gleam
let navn = "Per"
let alder = 27
io.write_err("%s er %i år gammel", navn, alder)
```

Dette vil skrive ut meldingen "Per er 27 år gammel" til stderr. 

# Dypdykk 

Det er viktig å huske at når man skriver til stderr, vil meldingene vises sammen med output fra standard out (oftest forkortet til stdout). Det kan derfor være nyttig å utnytte dette ved å skrive forskjellige typer meldinger til enten stderr eller stdout. Dette kan gjøres ved å bruke funksjonen `io.write_out()` for standard utput. 

# Se også 

- [Offisiell dokumentasjon for Gleam](https://gleam.run/documentation/)
- [Gleam Github repository](https://github.com/gleam-lang/gleam)