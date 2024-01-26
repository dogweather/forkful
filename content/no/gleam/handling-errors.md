---
title:                "Feilhåndtering"
date:                  2024-01-26T00:52:35.416507-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Behandling av feil handler om å forutse at ting kan gå galt i koden din og håndtere disse situasjonene med ynde. Programmerere gjør dette fordi det holder applikasjonene robuste og brukervennlige, selv når de står overfor det uventede.

## Hvordan:
I Gleam vil du ofte bruke `Result`-typen for feilhåndtering. Det er en enum med to varianter: `Ok` (for suksess) og `Error` (for feil). Her er et enkelt eksempel:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oops! Det gikk i stykker.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

Hvis du kjører `main` med `might_fail(False)`, vil den returnere `42`. Hvis du sender inn `True`, skriver den ut "Oops! Det gikk i stykker." og returnerer `0`.

## Dypdykk
Gleams tilnærming til feilhåndtering er påvirket av dens Erlang-røtter. Historisk sett bruker Erlang en "la det kræsje"-filosofi, som stoler på tilsynstrær for å håndtere prosessfeil. Imidlertid, når du skriver Gleam-kode som ikke er inne i en prosess som skal overvåkes, som i en bibliotekfunksjon, vil du håndtere feilene eksplisitt.

Alternativer til å bruke `Result` inkluderer bruk av `Option`-typen for tilfeller der noe kan være `None` (ingenting) eller `Some` (noe), men disse bærer ikke feilinformasjon. For å signalisere feil på tvers av prosessgrenser, kan du bruke Erlangs meldingsutvekslingsmekanismer.

Gleams feilhåndtering reflekterer en funksjonell programmeringsstil, der sideeffekter (som feil) håndteres med typer og mønsterpassing, og gir klarhet og forutsigbarhet i feilhåndteringen.

## Se også
- [Erlangs feilhåndtering](http://erlang.org/doc/reference_manual/errors.html)