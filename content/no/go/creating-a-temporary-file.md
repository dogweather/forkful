---
title:                "Go: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer er en viktig del av mange programmeringsprosjekter og kan være nyttig for å lagre midlertidige data eller gjøre midlertidige endringer i filer. Dette kan være nyttig for testing, debugging eller midlertidig lagring av data som ikke trengs på lang sikt.

## Hvordan

Å lage en midlertidig fil i Go er en enkel prosess som kan gjøres med få linjer kode. Først må vi importere biblioteket "io/ioutil". Deretter kan vi bruke funksjonen "ioutil.TempFile" til å opprette en midlertidig fil og lagre den i en variabel for senere bruk.

```Go
import "io/ioutil"

// Oppretter en midlertidig fil med prefiks "temp" og lagrer den i variabel
tempFil, err := ioutil.TempFile("", "temp")
if err != nil {
    // Håndter eventuelle feil
}

// Skriver data til den midlertidige filen
data := []byte("Dette er data som skal skrives til filen.")
err = ioutil.WriteFile(tempFil.Name(), data, 0644)
if err != nil {
    // Håndter eventuelle feil
}

// Lukker filen etter bruk
err = tempFil.Close()
if err != nil {
    // Håndter eventuelle feil
}

```

Etter at vi har opprettet og skrevet til den midlertidige filen, kan vi utføre våre midlertidige endringer eller testing, og deretter slette den med funksjonen "os.Remove". Det er viktig å huske å slette den midlertidige filen når vi er ferdige med den for å unngå å oppta unødvendig plass på datamaskinen.

```Go
import "os"

// Sletter den midlertidige filen etter bruk
err = os.Remove(tempFil.Name())
if err != nil {
    // Håndter eventuelle feil
}
```

## Dypdykk

I bakgrunnen lager Go en midlertidig fil på samme måte som den ville ha opprettet en ny fil, men den vil ha et unikt navn og bli automatisk slettet når programmet avsluttes eller filen lukkes. Vi kan også velge en spesifikk mappe hvor den midlertidige filen skal opprettes ved å legge til mappenavn som et argument i "ioutil.TempFile" funksjonen.

En annen viktig ting å merke seg er at når vi lager en midlertidig fil, vil den få et tilfeldig generert navn av Go, som kan være nyttig for å unngå sammenstøt med andre filer. Vi kan også angi et prefiks og suffiks for å legge til vår egen identifikator i filnavnet. Dette gjøres ved å legge til disse som separate argumenter i "ioutil.TempFile" funksjonen.

## Se også

- [Go dokumentasjon for midlertidige filer](https://golang.org/pkg/io/ioutil/#TempFile)
- [Go dokumentasjon for os.Remove](https://golang.org/pkg/os/#Remove)
- [En guide til midlertidige filer i Go](https://www.calhoun.io/creating-random-temp-files-in-go/)