---
title:                "Gleam: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen ønske å skrive en tekstfil? Vel, tekstfiler er nyttige for å lagre og organisere data som kan brukes i programmering. De er også enkle å opprette og kan brukes til en rekke formål, som å lage konfigurasjonsfiler eller generere liste med navn.

# Hvordan

For å skrive en tekstfil i Gleam, må du først importere modulen "File". Deretter kan du bruke "write" funksjonen for å opprette en tekstfil og skrive til den, som vist i koden nedenfor:

```Gleam
import File

pub fn main() {
  let filnavn = "minfil.txt"
  let data = "Dette er en tekstfil skrevet med Gleam."
  
  File.write(filnavn, data)
}
```

Når koden kjøres, vil det opprettes en tekstfil med navnet "minfil.txt" og teksten "Dette er en tekstfil skrevet med Gleam" vil bli skrevet til filen.

# Dypdykk

Å skrive en tekstfil i Gleam er enkelt, men det er også en rekke andre funksjoner du kan bruke for å tilpasse filene dine ytterligere. For eksempel kan du bruke "append" funksjonen for å legge til ny tekst i en eksisterende fil. Du kan også bruke "read" funksjonen for å lese innholdet av en tekstfil.

Det finnes også en rekke andre moduler og funksjoner som kan hjelpe deg med å håndtere tekstfiler, som for eksempel "Path" modulen for å arbeide med filstier og "IO" modulen for å håndtere inndata og utdata.

# Se også

- [Gleam dokumentasjon](https://gleam.run/documentation)
- [File.modul dokumentasjon](https://gleam.run/documentation/stdlib/file/)
- [Tutoriat for å skrive tekstfiler i Gleam](https://github.com/gleam-lang/gleam/blob/master/docs/tutorials/writing_a_file.md)