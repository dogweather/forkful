---
title:                "Arbeid med csv"
html_title:           "Go: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noensinne har jobbet med store datasett, har du kanskje sett at disse datasettene ofte er lagret i CSV-format. CSV står for "Comma-separated values" og er en måte å organisere og lagre tabellignende data på. Å kunne håndtere og arbeide med CSV-filer er derfor en nyttig ferdighet for enhver Go-programmerer, spesielt for de som jobber med databehandling eller datavitenskap.

## Hvordan

For å kunne håndtere CSV i Go, trenger du å importere pakken "encoding/csv" ved å legge til følgende linje i koden din:

```Go
import "encoding/csv"
```

Deretter kan du bruke funksjonen "Open" for å åpne en CSV-fil på følgende måte:

```Go
file, err := os.Open("mydata.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()
```

Merk at du også trenger å håndtere eventuelle feil som kan oppstå ved å bruke "log.Fatal".

Neste trinn er å lese data fra CSV-filen og lagre den i en variabel. Dette kan gjøres ved å bruke "NewReader" funksjonen:

```Go
reader := csv.NewReader(file)
```

Du kan deretter bruke "Read" funksjonen for å lese hver linje av dataene i filen:

```Go
records, err := reader.Read()
if err != nil {
    log.Fatal(err)
}
```

Til slutt kan du behandle og manipulere dataene etter dine behov.

## Dykk dypere

Når du jobber med CSV i Go, er det viktig å være oppmerksom på formateringsreglene for denne typen filer. CSV-filer kan variere i formatering, og det er derfor viktig å forstå hvordan disse filene er strukturert for å kunne behandle dem riktig.

I tillegg kan det være nyttig å lære om og bruke ulike funksjoner og metoder som tilbys av "encoding/csv" pakken, for eksempel "ReadAll" for å lese hele filen på en gang, eller "Write" for å skrive data til en CSV-fil.

Å jobbe med CSV-filer kan også bli mer komplekst når du begynner å behandle store datasett. Det kan da være hensiktsmessig å se på andre pakker og biblioteker som kan hjelpe deg med å effektivisere og optimalisere datahåndteringen.

## Se også

- [Guide til å jobbe med CSV i Go](https://zetcode.com/golang/readcsv/)
- [Dokumentasjon for encoding/csv pakken i Go](https://golang.org/pkg/encoding/csv/)
- [Eksempelkode for å arbeide med CSV-filer i Go](https://github.com/golang/go/wiki/CsvProcessing)