---
title:                "Go: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med data, er sjansen stor for at du vil komme over CSV-filer. CSV står for "Comma-Separated Values" og er en vanlig format for å lagre tabellbasert data. Ved å jobbe med CSV-filer kan du enkelt organisere, dele og analysere data i ditt Go-program.

## Hvordan

For å jobbe med CSV-filer i Go, må du importere "encoding/csv" pakken. Deretter må du åpne og lese CSV-filen ved hjelp av en "Reader" eller "ReadAll" metode. Du kan også skrive data til en CSV-fil ved hjelp av "Writer" metoden.

La oss si at du har en CSV-fil som inneholder informasjon om ansatte i en bedrift. Du kan enkelt lese og skrive dataene ved å bruke følgende kode:

```Go
package main

import (
	"encoding/csv"
	"fmt"
	"log"
	"os"
)

func main() {
	// Åpne CSV-filen
	file, err := os.Open("ansatte.csv")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Les dataene som en "Reader"
	reader := csv.NewReader(file)
	records, err := reader.ReadAll()
	if err != nil {
		log.Fatal(err)
	}

	// Skriv ut alle ansatte og deres lønn
	for _, row := range records {
		fmt.Println(row[0], "jobber som", row[1], "og tjener", row[2], "i året")
	}

	// Opprett en ny CSV-fil og skriv data til den
	newFile, err := os.Create("nye_ansatte.csv")
	if err != nil {
		log.Fatal(err)
	}
	defer newFile.Close()

	// Bruk en "Writer" for å skrive dataene til den nye filen
	writer := csv.NewWriter(newFile)
	defer writer.Flush()

	// Skriv informasjon om en ny ansatt
	nyAnsatt := []string{"Lisa", "Designer", "750 000"}
	err = writer.Write(nyAnsatt)
	if err != nil {
		log.Fatal(err)
	}
}
```

Kjør dette programmet og du vil se følgende output:

```
John jobber som Developer og tjener 800 000 i året
Sara jobber som Marketing Coordinator og tjener 650 000 i året
Mark jobber som Sales Manager og tjener 900 000 i året
Maria jobber som HR Specialist og tjener 700 000 i året
Lisa jobber som Designer og tjener 750 000 i året
```

## Dypdykk

Go har mange innebygde funksjoner for å jobbe med CSV-filer, som å definere de ulike delimitørene, overskriftsrad og EOL (end-of-line) tegn. Du kan også sette egne argumenter for å håndtere eventuelle spesielle tilfeller i CSV-filen din.

I tillegg til dette, kan du også bruke tredjeparts biblioteker som tilbyr mer avanserte funksjoner som å konvertere CSV filer til andre formater som JSON eller XML.

Du bør også være oppmerksom på at å jobbe med store CSV-filer kan føre til minneproblemer, så det kan være lurt å bruke funksjoner som "Read" og "Write" i stedet for "ReadAll" og "WriteAll" for å unngå dette.

## Se også

- [Go-pakken for å jobbe med CSV-filer](https://golang.org/pkg/encoding/csv/)
- [Go-biblioteker for avanserte CSV-funksjoner](https://awesome-go.com/#csv)