---
title:                "Een nieuw project starten"
date:                  2024-01-28T22:08:25.659645-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een nieuw project starten betekent het opzetten van de basis voor je Go applicatie. Programmeurs doen dit om code te organiseren, afhankelijkheden te beheren en het podium klaar te zetten voor verdere ontwikkeling.

## Hoe:
Installeer eerst Go, als je dat nog niet hebt gedaan, vanaf [golang.org](https://golang.org/dl/). Zet vervolgens een nieuw project op:

1. Open een terminal.
2. Maak een nieuwe map.

   ```bash
   mkdir mijnproject
   cd mijnproject
   ```

3. Initialiseer de module:

   ```bash
   go mod init github.com/uwgebruikersnaam/mijnproject
   ```

4. Schrijf een eenvoudig `main.go` bestand:

   ```Go
   package main

   import "fmt"

   func main() {
       fmt.Println("Hallo, nieuwe wereld van Go!")
   }
   ```

5. Voer het programma uit:

   ```bash
   go run main.go
   ```

De voorbeelduitvoer zou moeten zijn:

```
Hallo, nieuwe wereld van Go!
```

## Diepere Duik
Een nieuw project starten in Go is geëvolueerd. Vroege Go-projecten hadden geen officieel pakketbeheersysteem. Dit leidde tot het "GOPATH" werkruimtemodel, wat rommelig kon worden met grotere projecten. Tegenwoordig, met de introductie van `go mod` in Go 1.11, zijn zaken meer gestroomlijnd en beheersbaar: afhankelijkheden worden per project gehanteerd, niet globaal.

Alternatieven voor `go mod` vervagen, maar omvatten gemeenschapstools zoals `dep` en `glide`. Tegenwoordig is `go mod` de aanbevolen tool vanwege de eersteklas ondersteuning en integratie met de Go-toolchain.

Wanneer je `go mod init` uitvoert, maakt Go een nieuw `go.mod` bestand. Dit bestand houdt de afhankelijkheden van je project bij. Het vermeldt automatisch de versie van Go en eventuele externe pakketten die je later toevoegt. Met deze opzet zijn de afhankelijkheden van je code expliciet en reproduceerbaar, wat helpt om het "werkt op mijn machine" syndroom te vermijden.

## Zie Ook
- [Aan de slag met Go](https://golang.org/doc/install)
- [Hoe schrijf je Go code](https://golang.org/doc/code.html)
- [`go mod` Documentatie](https://golang.org/ref/mod)
