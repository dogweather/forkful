---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Konvertere en Dato til en Streng i Go

## Hva & Hvorfor?
Konvertering av en dato til en streng i programmering betyr å endre en dato fra dens opprinnelige datatype til en lesbar streng (tekst). Dette er nyttig for å vise datoer på en brukervennlig måte, eller for å lagre dato / tid i databaser som tekst.

## Hvordan gjør man det:
Her er et eksempel på hvordan man kan konvertere en dato til en streng i Go.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println(t.Format("2006-01-02"))
}
```
Når du kjører koden vil den skrive ut dagens dato i formatet "YYYY-MM-DD".

## Dypdykk
I Go bruker vi `Format` metoden fra `time` pakken for å konvertere datoer til strenger. Funksjonen tar en layout streng som argument, som definerer ønsket format av datoen.

Historisk sett bruker Go en uvanlig dato layout på grunn av dens høy kompatibilitet og fleksibilitet. "2006-01-02" er Go sin layout for å representere formatet "YYYY-MM-DD". Dette systemet tillater en mer intuitiv tilnærming enn mange andre programmeringsspråk.

Et alternativ er å bruke `Sprint` eller `Sprintf` fra `fmt` pakken, men `Format` gir mer kontroll over det endelige formatet av datoen.

## Se Også
For mer informasjon om emnet, ta en titt på følgende ressurser:
- Offisiell Go dokumentasjon på pakken 'time': [https://pkg.go.dev/time](https://pkg.go.dev/time)
- Go sin tutorial om dato og tid formatering: [https://yourbasic.org/golang/format-parse-string-time-date-example/](https://yourbasic.org/golang/format-parse-string-time-date-example/)
- Stack Overflow tråd om emnet: [https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format](https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format)