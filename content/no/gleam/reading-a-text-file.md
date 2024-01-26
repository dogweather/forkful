---
title:                "Lese en tekstfil"
date:                  2024-01-20T17:54:12.948729-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? - Hva & Hvorfor?
Lesing av tekstfiler handler om å hente og bruke data lagret på et lesbart format. Programmerere gjør dette for å behandle informasjon og for å integrere eksterne data i applikasjonene sine.

## How to - Hvordan:
I Gleam kan du lese en tekstfil med standard bibliotekfunksjoner. Her er et eksempel:

```gleam
import gleam/io

pub fn main() {
  let result = io.read_file("hello.txt")
  case result {
    Ok(content) -> io.println(content)
    Error(error) -> io.println("Oops! Noe gikk galt: " ++ error)
  }
}
```

Dersom `hello.txt` inneholder "Hei, verden!", vil utskriften bli:

```
Hei, verden!
```

## Deep Dive - Dypdykk
Å lese tekstfiler er en grunnleggende operasjon som har eksistert siden de tidlige dagene av programmering. Alternativer includerer streaming og asynkron lesing for store eller eksterne filer. Implementasjonsdetaljer varierer mellom operativsystemer og programmeringsspråk, men de fleste moderne språk tilbyr abstraksjoner for å håndtere filsystemet.

I Gleam's tilfelle, bruker `io.read_file` en enkel synkron tilnærming, som passer for små filer og skript. For større filer kan man vurdere streaming for å unngå høyt minneforbruk.

## See Also - Se Også
For videre lesing og beslektede emner, sjekk ut følgende ressurser:

- Erlang's file handling (Gleam bygger på Erlang's VM): [Erlang File Module](http://erlang.org/doc/man/file.html)
