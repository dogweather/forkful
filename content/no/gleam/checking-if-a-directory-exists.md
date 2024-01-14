---
title:                "Gleam: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av programmering. Det kan hjelpe deg med å sikre at programmet ditt fungerer riktig og unngå feil i koden.

## Hvordan

### Sjekke eksistensen av en mappe
```Gleam
import gleam/path.Dir

fn check_directory() {
    let dir = Dir.from_path("./my_directory")
    let exists = Dir.exists(dir)
    if exists {
        io.print("Mappen eksisterer.")
    } else {
        io.print("Mappen eksisterer ikke.")
    }
}

check_directory() // output: Mappen eksisterer.
```

### Sjekke eksistensen av en mappe og dens innhold
```Gleam
import gleam/path.Dir

fn check_directory_contents() {
    let dir = Dir.from_path("./my_directory")
    let exists = Dir.exists(dir)
    let contents = Dir.contents(dir)
    if exists {
        io.print(contents)
    } else {
        io.print("Mappen eksisterer ikke.")
    }
}

check_directory_contents() // output: [{name: "file1.txt", path: "./my_directory/file1.txt"}, {name: "file2.txt", path: "./my_directory/file2.txt"}]
```

## Dypdykk

Når vi sjekker eksistensen av en mappe, må vi huske på at det også er viktig å håndtere eventuelle feil som kan oppstå. For eksempel kan det hende at mappen ikke eksisterer eller at vi ikke har tilgang til den.

For å håndtere slike scenarier kan vi bruke try/catch-metoden:
```Gleam
import gleam/path.Dir

fn check_directory() {
    try {
        let dir = Dir.from_path("./my_directory")
        let exists = Dir.exists(dir)
        if exists {
            io.print("Mappen eksisterer.")
        } else {
            io.print("Mappen eksisterer ikke.")
        }
    } catch error {
        io.print("En feil oppsto. Feilmelding: " ++ error)
    }
}

check_directory() // output: En feil oppsto. Feilmelding: "Directory does not exist."
```

## Se også

- [Gleam dokumentasjon for å sjekke eksistensen av en mappe](https://gleam.run/functions/Dir.exists.html)
- [Tutorial for å håndtere feil i Gleam](https://gleam.run/tutorials/error_handling.html)