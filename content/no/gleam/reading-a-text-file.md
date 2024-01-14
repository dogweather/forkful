---
title:                "Gleam: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil kan være en viktig del av programmering, spesielt når du jobber med store datamengder eller trenger å hente innhold fra en ekstern kilde. Det kan spare deg for mye tid og krefter ved å automatisere lesingen av filer i kode.

## Hvordan Gjøre Det

Med Gleam programmeringsspråk, kan du enkelt lese en tekstfil ved hjelp av "File" biblioteket. Først må du importere dette biblioteket ved å skrive:

```Gleam
import gleam/file
```

Deretter kan du åpne en tekstfil ved å bruke "file.open" funksjonen og oppgi banen til filen som et argument. For eksempel:

```Gleam
file.open("minfil.txt")
```

Dette vil returnere en filobjekt som du kan bruke til å lese og manipulere innholdet i filen. Du kan bruke funksjonen "file.read" for å lese hele innholdet i filen, eller "file.read_lines" for å lese linje for linje.

For å demonstrere dette i praksis, la oss si at vi har en tekstfil med navnet "greeting.txt" som inneholder følgende tekst:

```
Hei! Velkommen til min blogg.
```

Vi kan da lese innholdet i denne filen og skrive det ut ved hjelp av følgende kode:

```Gleam
let file = file.open("greeting.txt")
let content = file.read_lines()
```

Dette vil returnere en liste med en eneste verdi, nemlig linjen som vi leste fra filen. For å skrive ut denne linjen, kan du bruke "io.format" funksjonen som følger:

```Gleam
let file = file.open("greeting.txt")
let content = file.read_lines()
io.format("Here is the content of greeting.txt: {}", [content])
```

Dette vil skrive ut følgende output:

```
Here is the content of greeting.txt: Hei! Velkommen til min blogg.
```

## Dypdykk

Det er også mulig å lese en tekstfil rad for rad og utføre forskjellige operasjoner på hvert avsnitt. Dette kan være nyttig når du for eksempel ønsker å gjøre noe spesifikt med hver linje i en CSV-fil eller et loggfil.

For å lese filen rad for rad, kan du bruke "file.each_line" funksjonen og en lambda-funksjon. For eksempel:

```Gleam
let file = file.open("navn.txt")

file.each_line(fn line -> {
  io.format("Hei, {}! Velkommen til min blogg.", [line])
})
```

Dette vil skrive ut følgende for hver linje i filen:

```
Hei, Ane! Velkommen til min blogg.
Hei, Markus! Velkommen til min blogg.
Hei, Emilie! Velkommen til min blogg.
```

## Se Også

- [Gleam File Bibliotek](https://gleam.run/documentation/standard-library/file)
- [Offisiell Gleam Dokumentasjon](https://gleam.run/documentation/)