---
title:                "Go: Å skrive en tekstfil."
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan være en nyttig oppgave for utviklere, spesielt i Go-programmering. Det lar oss lagre og organisere data på en enkel og lett tilgjengelig måte. I denne bloggposten vil vi se nærmere på hvordan man skriver en tekstfil i Go.

## Hvordan

For å skrive en tekstfil i Go, må vi først opprette en filobjekt og angi ønsket filnavn og plassering. Dette gjøres ved hjelp av `os.Create()` -funksjonen, som tar inn navnet på filen som argument og returnerer et filobjekt.

```Go
f, err := os.Create("tekstfil.txt")

if err != nil {
  fmt.Println(err)
}

defer f.Close()
```

Vi bruker `defer` -nøkkelordet for å sørge for at filen blir lukket etter at vi har gjort operasjoner på den.

Neste steg er å skrive innholdet til filen. Dette gjøres ved å bruke `WriteString()` -metoden til filobjektet. Denne metoden tar inn en streng som argument og skriver den til filen.

```Go
_, err = f.WriteString("Hei verden!")

if err != nil {
  fmt.Println(err)
}
```

For å lagre endringene og lukke filen, bruker vi `Sync()` -metoden.

```Go
err = f.Sync()

if err != nil {
  fmt.Println(err)
}
```

Nå har vi en fullstendig tekstfil med innholdet "Hei verden!" lagret på den angitte plasseringen.

## Dypdykk

Ved å bruke `Write()` -metoden i stedet for `WriteString()` kan vi skrive binære data til tekstfilen. Dette lar oss lagre mer kompleks informasjon som for eksempel tall eller boolske verdier.

Vi kan også inkludere mer avanserte funksjoner som å skrive til bestemte linjer i filen eller å skrive til filen i tillegg til å legge til nytt innhold.

## Se også

- [Go offisiell dokumentasjon for filbehandling](https://golang.org/pkg/os/)
- [En grundig guide til filbehandling i Go](https://tutorialedge.net/golang/reading-writing-files-in-go/)
- [En oversikt over ulike måter å skrive til fil i Go](https://www.calhoun.io/writing-to-files-in-go/)