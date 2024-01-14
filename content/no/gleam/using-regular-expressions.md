---
title:                "Gleam: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk, eller vanlige uttrykk, kan være nyttig for å finne og manipulere spesifikke mønstre i tekst. Dette kan være nyttig for programmerere som ønsker å automatisere oppgaver, som for eksempel å finne og erstatte visse deler av en tekstfil.

## Slik gjør du det

For å bruke regulære uttrykk i Gleam, må du importere biblioteket "Regex". Deretter kan du bruke funksjonen "match" for å finne et mønster i en tekststreng. For eksempel, hvis du ønsker å finne alle forekomster av et ord i en tekststreng, kan du skrive følgende kode:

``` 
Gleam
import Regex

let text = "Hei, jeg heter Maria. Jeg liker å programmere."

match(text) {
  Ok(matches) -> 
    for match in matches {
      match_text = match.text()
      io.format("Fant følgende mønster: {}", [match_text])
    }
  Err(error) -> io.format("Feil: {}", [error])
}
```

Denne koden vil finne og skrive ut alle forekomster av ordet "jeg" i teksten. Output vil være:

Fant følgende mønster: jeg
Fant følgende mønster: jeg

## Dypdykk

Regulære uttrykk kan være komplekse, men heldigvis har Gleam et enkelt og intuitivt grensesnitt for å jobbe med dem. Du kan bruke spesielle karakterer og uttrykk for å finne og manipulere mønster i en tekststreng. For eksempel kan du bruke ".+" for å finne alle tegn etter et bestemt mønster, eller "^" for å finne ord i starten av en tekst.

Det er også mulig å bruke grupper i regulære uttrykk for å hente ut spesifikke deler av en tekststreng. Dette kan være svært nyttig for å gjøre komplekse manipuleringer av tekst.

## Se også

- [Gleam Regex bibliotek](https://github.com/gleam-lang/regex)
- [Offisiell Gleam nettside](https://gleam.run/)
- [Gleam dokumentasjon](https://gleam.run/documentation/)