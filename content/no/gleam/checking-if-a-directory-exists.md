---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:56:26.980091-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe eksisterer betyr å verifisere at en bestemt sti på filsystemet ditt er tilgjengelig og peker til en mappe. Programmerere gjør dette for å unngå feil ved filoperasjoner, som å lese fra eller skrive til ikke-eksisterende kataloger.

## Hvordan:
Gleam-koden for å sjekke en mappe er ikke innebygd i standardbiblioteket, men vi kan bruke Erlang funksjoner ved hjelp av `gleam/erlang` biblioteket. Her er et eksempel:

```gleam
import gleam/erlang
import gleam/io

pub fn check_dir_exists(dir: String) -> Bool {
  erlang.is_directory(dir)
}

pub fn main() {
  let dir_exists = check_dir_exists("min_mappe")
  io.println(dir_exists)
}
```

Kjører du dette, vil utskriften være enten `True` hvis mappen eksisterer eller `False` hvis den ikke gjør det.

## Dypdykk:
Før i tiden var filoperasjoner hovedsakelig håndtert av operativsystemene direkte. Språk som Java og Python introduserte standardiserte biblioteker for filsystemoperasjoner. I Gleam, som er et relativt nytt språk bygget på Erlang VM, bruker vi ofte Erlang funksjoner direkte for filsystemrelaterte oppgaver.

Alternativer til å håndtere dette kan være å bruke en ekstern crate (Rust-term for bibliotek) eller skrive egen funksjonalitet som direkte snakker med operativsystemet via porter.

Implementeringsmessig er en sjekk på en mappe i mange språk en systemkall som sjekker filsystemets tabell for oppføringer som matcher stien. På Unix-lignende systemer gjøres dette ved `stat()` eller lignende systemkall.

## Se Også:
- Erlang dokumentasjon på filsystemoperasjoner: [Erlang File module](http://erlang.org/doc/man/file.html)
- Gleam dokumentasjon og spesifikasjoner: [Gleam Book](https://gleam.run/book)
- Diskusjoner om filsystemhåndtering i Gleam: [Gleam’s GitHub issues](https://github.com/gleam-lang/gleam/issues)
