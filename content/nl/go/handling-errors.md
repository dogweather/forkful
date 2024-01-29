---
title:                "Fouten afhandelen"
date:                  2024-01-28T22:01:56.433929-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/handling-errors.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling in Go gaat over het sierlijk opvangen en reageren op runtime storingen. We doen dit om crashes te voorkomen en ervoor te zorgen dat onze programma's voorspelbaar handelen, zelfs wanneer dingen misgaan.

## Hoe:

Go gebruikt expliciete foutafhandeling. Dat betekent dat je elke keer controleert of een functie een fout retourneert wanneer je deze aanroept. Geen uitzonderingen. Zo ziet dat eruit:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doeIets()
	if err != nil {
		fmt.Println("Uh oh:", err)
		os.Exit(1)
	}
}

func doeIets() error {
	// Doen alsof er iets misging
	return fmt.Errorf("er ging iets mis")
}
```

Voer dit uit, en je krijgt:

```
Uh oh: er ging iets mis
```

Maar wat als het slaagt?

```Go
func doeIets() error {
	// Deze keer is alles goed
	return nil
}
```

Geen output. Cool, geen nieuws is goed nieuws.

## Diepere duik:

In Go is foutafhandeling een punt van discussie geweest. Sinds het begin koos Go tegen uitzonderingen voor een meer expliciete aanpak, waar sommige ontwikkelaars van houden vanwege de eenvoud en anderen het omslachtig vinden. Het ingebouwde `error` type is een interface. Elk type met een `Error() string` methode voldoet eraan. Dit sluit aan bij de Go ethos van eenvoud en explicietheid.

Alternatieven? Er is het duo `panic` en `recover`, maar die zijn voor uitzonderlijke gevallen (woordspeling bedoeld) wanneer het programma niet kan doorgaan. Denk aan `panic` als de eject knop die je indrukt wanneer je weet dat er geen terugkomst mogelijk is. Gebruik het met mate.

Wat betreft reguliere foutafhandeling, introduceerde Go 1.13 foutverpakking, waardoor het gemakkelijker is om de "foutenketen" uit te zoeken met functies zoals `errors.Is()` en `errors.As()`.

## Zie ook:

Voor alles over foutafhandeling in Go:

- De Go Blog over foutafhandeling: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go - Sectie over foutafhandeling: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 Foutverpakking documentatie: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheney's post over strategieën voor foutafhandeling: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
