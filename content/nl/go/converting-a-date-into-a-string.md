---
title:                "Een datum converteren naar een string"
date:                  2024-01-28T21:57:21.916889-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het converteren van een datum naar een tekenreeks betekent het wijzigen van een datumformaat van een door de computer gebruikt formaat naar een formaat dat mensen makkelijk kunnen lezen. Programmeurs doen dit om datums op interfaces weer te geven of om ze te formatteren voor rapporten en logboeken.

## Hoe te:
In Go is het omzetten van een datum naar een tekenreeks vrij eenvoudig met het `time` pakket.

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	huidigeTijd := time.Now()
	fmt.Println("Geformatteerde Datum:", huidigeTijd.Format("2006-01-02 15:04:05"))
}
```

Uitvoer ziet er mogelijk zo uit:
```
Geformatteerde Datum: 2023-04-07 14:21:34
```

De `Format` methode gebruikt een speciale referentiedatum: Mon Jan 2 15:04:05 MST 2006. Je stemt je gewenste formaat af op de lay-out van deze referentiedatum. Slimme truc, toch?

## Diepgaande Duik
Go's `time` pakket behandelt datum- en tijdbewerkingen. De `Format` methode van de `time.Time` struct is een werkpaard.

Waarom de vreemde referentiedatum "2006-01-02 15:04:05"? In de beginjaren van Go was dit patroon gekozen omdat de getallen (1 tot en met 7) elk uniek zijn en met 1 toenemen, dus elk vertegenwoordigt een ander component van het tijdformaat. Het maakt het eigenzinnig maar intuïtief zodra je het begrijpt.

Alternatieven? Zeker, we hebben externe bibliotheken zoals `timeparse` of `strftime` die de tijdafhandeling van andere talen nabootsen. Maar voor de meesten van ons volstaat de standaardbibliotheek prima.

Achter de schermen omvat formatteren het parsen van de lay-out van de referentietijd en het vervangen van delen met overeenkomstige waarden uit de daadwerkelijk geformatteerde tijd. Het handelt ook tijdzoneconversies af - een must voor wereldwijde apps.

## Zie Ook
Voor een diepgaande duik in het Go `time` pakket, bekijk:
- De officiële documentatie op https://pkg.go.dev/time
- Go by Example's kijk op datumformattering: https://gobyexample.com/time-formatting-parsing

Wanneer stack overflow toeslaat, kunnen deze threads een redder in nood zijn:
- Tijd Formatteren: https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format
- Strings Omzetten naar Tijd: https://stackoverflow.com/questions/14106541/how-to-parse-date-string-in-go
