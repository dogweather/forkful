---
title:                "Een datum in de toekomst of het verleden berekenen"
date:                  2024-01-28T21:55:36.092219-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het berekenen van een datum in de toekomst of het verleden is precies wat het klinkt als - uitzoeken welke datum het zal zijn, bijvoorbeeld 10 dagen vanaf nu, of welke datum het was 50 dagen geleden. Programmeurs doen dit voor zaken zoals het vaststellen van deadlines, vervaldatums of het beheren van reserveringen.

## Hoe:

Laten we met tijd spelen in Go:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// De datum van vandaag
	vandaag := time.Now()
	fmt.Println("Vandaag is:", vandaag.Format("Jan 2, 2006"))

	// Toekomstige datum, 10 dagen vanaf nu
	toekomst := vandaag.Add(10 * 24 * time.Hour)
	fmt.Println("10 dagen vanaf nu:", toekomst.Format("Jan 2, 2006"))

	// Verleden datum, 50 dagen geleden
	verleden := vandaag.Add(-50 * 24 * time.Hour)
	fmt.Println("50 dagen geleden:", verleden.Format("Jan 2, 2006"))
}
```

Voer het uit en je zult zoiets zien:

```
Vandaag is: Mrt 15, 2023
10 dagen vanaf nu: Mrt 25, 2023
50 dagen geleden: Jan 24, 2023
```

## Diep Duiken

Waarom zijn data belangrijk? Welnu, historisch gezien is het bijhouden van tijd essentieel geweest voor landbouw, wetenschap, geschiedenis, noem maar op. In de informatica is het net zo cruciaal - denk aan taken zoals back-ups, vervaldatumcontroles en planning.

Voor Go's `time` pakket moesten we vertrouwen op minder intuïtieve bibliotheken of, god verhoede, handmatige berekeningen. Nu kunnen we datums manipuleren met `Add` om duuraties toe te voegen, of `Sub` om de duur tussen twee datums te vinden.

Ook, hier is een leuk weetje: berekeningen houden rekening met schrikkeljaren en dergelijke, maar er is geen afhandeling voor eigenaardigheden in door mensen gemaakte kalenders (zoals toen Groot-Brittannië in 1752 11 dagen oversloeg).

Alternatieven? Zeker. Je zou `AddDate` kunnen gebruiken om specifieke aantallen jaren, maanden en dagen toe te voegen, als je de aanpak van `duur * time.Hour` niet prettig vindt.

Wat de implementatie betreft, gebruikt Go een proleptische Gregoriaanse kalender, uitgebreid terug tot het jaar één en vooruit naar de verre toekomst. Het is hetzelfde systeem dat we dagelijks gebruiken, minus de eigenaardigheden van historische kalenderhervormingen.

## Zie Ook

- De Go Programmeringstaal Specificatie over tijd: https://golang.org/ref/spec#Time
- De Go `time` pakket documentatie: https://pkg.go.dev/time
- Rob Pike's lezing over Go's Tijd Formaat: https://www.youtube.com/watch?v=rKnDgT73v8s
