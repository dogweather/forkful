---
title:                "Een tekstbestand schrijven"
date:                  2024-01-28T22:12:27.267878-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand schrijven"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het schrijven van een tekstbestand betekent het opslaan van gegevens in een bestand dat tekst bevat, meestal in een door mensen leesbaar formaat zoals `.txt` of `.csv`. Programmeurs schrijven bestanden om gegevens te bewaren en persistent te maken, die later door mensen gelezen of door andere programma's gebruikt kunnen worden.

## Hoe:

Hier is hoe je een string naar een tekstbestand schrijft in Go:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	message := "Hallo, Go!"

	file, err := os.Create("voorbeeld.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	_, err = file.WriteString(message)
	if err != nil {
		log.Fatal(err)
	}

	log.Println("Bestand succesvol geschreven!")
}
```

Voer het uit. Als het succesvol is, zal je geen fouten zien, maar wordt `voorbeeld.txt` gecreëerd.

## Diepe Duik

Het schrijven naar tekstbestanden in Go gebruikt het `os`-pakket, dat een platformonafhankelijke interface biedt voor functionaliteit van het besturingssysteem. De functie `os.Create` maakt of truncates een bestand. De methode `File.WriteString` is rechttoe rechtaan voor het schrijven van strings.

Historisch gezien is de afhandeling van tekstbestanden geëvolueerd uit de `stdio.h` bibliotheek van C. In Go is eenvoud de sleutel; je doet minder voor meer actie, en vermijdt boilerplate. Alternatieven zoals `ioutil.WriteFile` bestaan maar worden niet geadviseerd voor grote bestanden vanwege geheugeninefficiëntie. `bufio` biedt gebufferde I/O, wat systeemaanroepen vermindert en de prestaties verbetert.

## Zie Ook

- Go by Example: Bestanden Schrijven: https://gobyexample.com/writing-files
- Go Documentatie voor het os-pakket: https://pkg.go.dev/os
- Go `bufio`-pakket: https://pkg.go.dev/bufio
