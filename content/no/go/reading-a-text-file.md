---
title:    "Go: Lesing av tekstfil"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor 

Lesing av tekstfiler er en vanlig oppgave for mange utviklere, enten det er for å behandle og analysere store datamengder, eller bare for å lese innholdet i en konfigurasjonsfil. Uansett grunn er det viktig å kunne lese tekstfiler effektivt for å kunne lage robuste og funksjonelle programmer. I denne bloggposten vil jeg forklare hvordan du kan lese tekstfiler ved hjelp av Go-programmeringsspråket. 

## Hvordan 
For å lese en tekstfil i Go, må du først åpne filen ved hjelp av `os.Open()`-funksjonen som tar inn filbanen som parameter. Deretter bruker vi `bufio.NewScanner()`-funksjonen for å skanne gjennom filen linje for linje. I dette eksempelet vil vi lese en tekstfil og skrive ut hvert linjeinnhold til konsollen. 

```Go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	// Åpner tekstfilen
	file, err := os.Open("tekstfil.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Skanner gjennom filen
	scanner := bufio.NewScanner(file)

	// Itererer over hver linje i tekstfilen
	for scanner.Scan() {
		// Skriver ut linjeinnholdet
		fmt.Println(scanner.Text())
	}

	// Sjekker om det var noen feil under lesing av filen
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
```

### Eksempel tekstfil (`tekstfil.txt`):

```
Dette er en tekstfil.
Her kan du se eksempeltekst.
```

### Eksempel output:
```
Dette er en tekstfil.
Her kan du se eksempeltekst.
```

## Dypdykk 
Det finnes flere måter å lese og behandle tekstfiler i Go på, som å bruke `ioutil.ReadFile()`-funksjonen eller `os.ReadFile()`-funksjonen. Det er også mulig å spesifisere en spesifikk del av tekstfilen du vil lese ved hjelp av `io.ReadSeeker`-grensesnittet. Videre kan du også behandle tekstfiler ved å bruke `strings.Split()`-funksjonen for å dele opp tekstfilen i mindre biter basert på en spesifikk separator, for eksempel komma eller tabulator. 

## Se også 
- https://golang.org/pkg/bufio/#Scanner
- https://golang.org/pkg/os/#File
- https://golang.org/pkg/io/#Copy 
- https://pkg.go.dev/io#ReadSeeker
- https://pkg.go.dev/strings#Split