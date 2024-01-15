---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Gleam: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lese kommandolinjeargumenter kan være en nyttig ferdighet å ha for utviklere som arbeider med terminalbaserte programmer eller scripts. Dette gjør at de kan tilpasse og kontrollere hvordan programmet deres fungerer uten å måtte endre selve koden.

# Hvordan

For å få tilgang til kommandolinjeargumenter i Gleam kan du bruke funksjonen `CommandLine.arguments()`, som gir deg en liste med alle argumentene som ble gitt da programmet ble kjørt.

```Gleam
import gleam/cli

pub fn main() {
  let arguments = CommandLine.arguments()
  // gjør noe spennende med argumentene her
}
```

La oss si at du kjører programmet ditt med `gleam run hello_world.gleam --name Bob`. Da vil `CommandLine.arguments()` returnere en liste med verdier `[hello_world.gleam, --name, Bob]`.

# Deep Dive

Det finnes også muligheter for å gi verdier til argumentene dine ved hjelp av flagg og navngitte argumenter. Dette kan gjøres ved å bruke `gleam/cli`-modulen og dens funksjoner som `get_flag()` og `get_named_arg()`.

`get_flag()` brukes for flagg som ikke har noen verdi, for eksempel `--help`. Mens `get_named_arg()` brukes for å hente verdien til et navngitt argument, for eksempel `--name Bob`.

For å bruke disse funksjonene må du først definere hvilke flagg og navngitte argumenter du forventer, som vist i eksempelet nedenfor.

```Gleam
import gleam/cli

config fn main(flags, args) {
  flags
    | get_flag(_, "--help")
    | get_named_arg(_, "--name", "Bob")
  // gjør noe magisk med flagg og argumenter her
}
```

# Se også

- [Gleam dokumentasjon](https://gleam.run/documentation/)
- [Gleam Github-repositorium](https://github.com/gleam-lang/gleam)