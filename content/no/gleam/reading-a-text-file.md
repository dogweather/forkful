---
title:    "Gleam: Lesing av tekstfil"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Hvorfor

Å lese tekstfiler er en viktig del av mange programmeringsoppgaver. Det kan være nyttig for å hente data fra eksterne kilder, analysere tekstbaserte filer som CSV eller XML, eller bare for å få en bedre forståelse av hvordan tekstbehandling fungerer. Uansett hva årsaken er, kan du med Gleam gjøre dette raskt og enkelt.

## Hvordan

Det første trinnet for å lese en tekstfil med Gleam er å åpne filen. Dette gjøres ved å bruke funksjonen `gleam/io.open()` med filnavnet som argument. Deretter kan du lese filen linje for linje ved å bruke en `while`-løkke. Inne i løkken brukes funksjonen `gleam/io.read_line()` for å lese en linje om gangen, og med `gleam/io.print()` kan du skrive ut linjene på skjermen. Ta en titt på følgende kodeeksempel:

```Gleam
let file = gleam/io.open("tekstfil.txt")
while line = gleam/io.read_line(file) {
  gleam/io.print(line)
}
gleam/io.close(file)
```

Dette eksempelet vil åpne en tekstfil med navnet "tekstfil.txt" og skrive ut hver linje på skjermen. Du kan også bruke `gleam/io.read_all()` for å lese hele filen som en enkelt streng, dersom det er ønskelig.

## Dypdykk

Gleam tilbyr også flere nyttige funksjoner for å håndtere tekstfiler. For eksempel kan du bruke `gleam/string.split()` for å dele en tekstfil i forskjellige deler basert på et gitt skilletegn, som komma eller pipe. Dette kan være nyttig når du jobber med CSV-filer.

En annen nyttig funksjon er `gleam/string.trim()`, som fjerner tomrom rundt en streng. Dette kan være nyttig når du leser og behandler data fra en tekstfil, da det ofte kan være uønskede mellomrom eller linjeskift i tekstfiler.

# Se også

- [Gleam dokumentasjon om I/O](https://gleam.run/articles/io)
- [Offisiell Gleam nettside](https://gleam.run/)
- [Gleam på GitHub](https://github.com/gleam-lang/gleam)