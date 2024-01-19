---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Gleam: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sjekke om en mappe eksisterer er en prosedyre der programmerere inspirer programvaren til å bestemme om en bestemt mappe finnes i systemet. Dette er viktig for å hindre feil som kan oppstå når programmet prøver å komme til mapper som ikke finnes.

## Hvordan: 

I Gleam kan vi bruke `Dir.exists` funksjonen for å sjekke om en mappe eksisterer. La oss se på et eksempel:

```Gleam
import gleam/fs/dir

fn main(args: List(String)) -> Nil {
  case dir.exists("eksisterer_mappe") {
    Ok(True) -> 
      io.println("Mappen eksisterer.")

    Ok(False) -> 
      io.println("Mappen eksisterer ikke.")
      
    Error(err) -> 
      io.println("Det er en feil: ", err)
  }
}
```

Når vi kjører programmet, kan utdataene være som følgende avhengig av om mappen eksisterer:

```
Mappen eksisterer.

eller 

Mappen eksisterer ikke.
```

## Dyp Dykk

Historisk sett har metoder for å sjekke om mapper eksisterer vært brukt i de fleste programmeringsspråk, skjønt med forskjellige implementasjoner. I Gleam, tilbyr `gleam/fs/dir`-modulen en høy-nivå grensesnitt for filsystemoperasjoner, inkludert sjekking av om en mappe eksisterer.

Alternativt, programmerere kan håndtere dette direkte med lav-nivå systemfunksjoner, men det ville øke kompleksiteten av koden. Økt kompleksitet kan føre til mer administrative belastning og mer feil.

Implementasjonsdetaljer: `Dir.exists` funksjonen brukes med et argument som representerer mappenavnet (en streng). Den returnerer et `Result` element hvor `Ok(True)` betyr at mappen eksisterer, `Ok(False)` betyr at mappen ikke eksisterer, og `Error` indikerer en utførelsesfeil.

## Se Også

For mer om Gleam's Filesystem operasjoner, se [Gleam's Filesystem doc](https://hexdocs.pm/gleam_stdlib/gleam/fs/dir.html).

For mer kontekst og sammenligning med andre programmeringsspråk, se ["Checking if a directory exists in Shell script"](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script) på StackOverflow.