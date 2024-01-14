---
title:                "Go: Lesing av tekstfiler"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å lese en tekstfil? Vel, for det første er tekstfiler en vanlig måte å lagre og organisere data på. Som en programmer er det nyttig å kunne lese og manipulere tekstfiler for å få tilgang til viktig informasjon.

Å lese tekstfiler er også en essensiell ferdighet for å utvikle Go programmer, siden mange programmer får input fra tekstfiler og produserer output til tekstfiler. Derfor er det viktig å forstå hvordan man leser en tekstfil i Go.

## Slik gjør du det

For å lese en tekstfil i Go, må vi først åpne filen ved hjelp av funksjonen `os.Open()`. Deretter kan vi bruke en buffer for å lese data fra filen. Etter å ha lest filen, må vi lukke den ved å kalle `file.Close()`.

Her er et eksempel på hvordan man kan lese en tekstfil og skrive ut innholdet til konsollen:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Åpner filen
	file, err := os.Open("tekstfil.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()

	// Lager en buffer for å lese data fra filen
	buffer := make([]byte, 1024)
	for {
		// Leser data fra filen inn i bufferet
		n, err := file.Read(buffer)

		// Avslutter dersom det ikke er mer data å lese
		if err != nil {
			fmt.Println("Ferdig lesing av filen")
			break
		}

		// Skriver ut dataen fra bufferet til konsollen
		fmt.Println(string(buffer[:n]))
	}
}
```

Output:
```
Dette er en tekstfil som skal leses.

Det finnes mange forskjellige eksempler på hvordan man kan lese tekstfiler i Go.
```

## Dypdykk

Å lese tekstfiler i Go kan virke enkelt, men det er noen ting å være oppmerksom på. For det første er data fra filer i binære formater og må derfor konverteres til tekst før vi kan lese det.

I tillegg må vi være oppmerksomme på hvor mye data som blir lest inn i bufferet vårt. Dersom filen er større enn størrelsen på bufferet, må vi fortsette å lese filen til den er helt lest.

Det er også verdt å nevne at det finnes mer avanserte måter å lese tekstfiler på ved hjelp av Go's `bufio` pakke, som inkluderer funksjoner som for eksempel `bufio.Scanner` for å gjøre lesingen enklere og mer effektiv.

## Se også

- [Les en tekstfil - Go dokumentasjon](https://golang.org/pkg/os/#Open)
- [Go's `bufio` pakke - Go dokumentasjon](https://golang.org/pkg/bufio/)