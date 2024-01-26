---
title:                "Opprette en midlertidig fil"
date:                  2024-01-20T17:40:12.407029-07:00
model:                 gpt-4-1106-preview
simple_title:         "Opprette en midlertidig fil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil betyr at du lager en fil som kun eksisterer for en kort stund, vanligvis under kjøring av et program. Programmerere gjør dette for å lagre data midlertidig uten å påvirke den permanente lagringen eller for å håndtere store datamengder som ikke passer i minnet.

## Slik gjør du det:
Gleam har ingen innebygd støtte for midlertidige filer ennå, men du kan bruke funksjoner fra underliggende systemer via FFI (Foreign Function Interface). Her er et eksempel ved bruk av Erlang-biblioteket `:file`:

```gleam
import gleam/erlang
import gleam/result

pub fn create_temp_file() -> result.Result(String, String) {
  erlang.apply_atom("file", "mktemp", ["tempXXXXXX"])
  |> result.map(fn(tuple) { tuple[1] })
  |> result.map_err(fn(_) { "Could not create temp file" })
}

pub fn main() {
  case create_temp_file() {
    Ok(filename) -> io.println("Temp file created: " ++ filename)
    Error(err) -> io.println("Error: " ++ err)
  }
}
```

Kjør koden og se etter noe lignende i utskriften:
```
Temp file created: tempWxA7K9
```

## Dypdykk
Opprettelsen av midlertidige filer stammer fra tiden med store flerbrukersystemer, der behovet for å holde data isolert og unngå navnekonflikter var stor. Alternativer inkluderer bruk av in-memory databaser som Redis, eller datastrukturer som buffere eller køer for midlertidig datalagring.

I Gleam kan vi for øyeblikket bruke Erlang- og Elixir-biblioteker for filhåndtering, siden Gleam kjører på BEAM (Erlang's virtuelle maskin). Dette gir deg tilgang til et robust sett med funksjonaliteter rundt filsystemet, selv om det betyr å stole på eksterne biblioteker frem til Gleam tilbyr slike funksjoner innebygd.

Implementeringsmessig lager `:file.mktemp` en unik filnavnprefiks med sekvensen "XXXXXX" som automatisk erstattes med tilfeldige karakterer for å sikre at filnavnet er unikt.

## Se Også
- Erlangs `:file` modul dokumentasjon: [erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Offisiell Gleam nettside: [gleam.run](https://gleam.run/)
