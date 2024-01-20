---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

---
title: Starte ditt første prosjekt med Gleam
---

## Hva & Hvorfor?

Å starte et nytt prosjekt er prosessen med å initialisere et nytt sett med kodefiler som er organisiert på en spesifikk måte. Programmerere gjør dette for å bygge en ny applikasjon eller et verktøy.

## Hvordan:

Å starte et nytt Gleam-prosjekt er enkelt. Bare bruk `gleam new` kommandoen og gi det et navn. 

```Gleam
$ gleam new mitt_prosjekt
```
Da får du en mappenstruktur som dette:

```Gleam
mitt_prosjekt
├── gleam.toml
├── README.md
├── src
│   ├── mitt_prosjekt.gleam
│   ├── mitt_prosjekt.app.src
├── test
│   ├── mitt_prosjekt_test.gleam
```

## Dyp Dykk

Gleam språket, selv om det er relativt nytt, har røtter i Erlang og Rust programmeringsspråk. Det er designet for å være svært uttrykksfullt og effektivt for både små og store prosjekter. 

Alternativene til å starte et nytt Gleam prosjekt kan være å klonere et eksisterende prosjekt, eller å bruke en boilerplate eller mal. 

Når du starter et nytt Gleam prosjekt, opprettes en rekke filer og kataloger som hjelper deg med å organisere koden din. Gleam bruker en modulær struktur for å organisere koden, noe som gjør det enkelt å strukturere din applikasjon eller bibliotek.

## Se Også:

- [Gleam's GitHub repo](https://github.com/gleam-lang/gleam)
- [Gleam koekkbok](https://github.com/gleam-lang/gleam-cookbook)
- [Offisiell dokumentasjon](https://gleam.run/docs/)