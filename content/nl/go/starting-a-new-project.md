---
title:                "Een nieuw project starten"
aliases:
- nl/go/starting-a-new-project.md
date:                  2024-02-03T18:09:31.934296-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/starting-a-new-project.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project opstarten in Go houdt in dat je een werkruimte opzet en deze initialiseert met de benodigde Go modules. Programmeurs doen dit om code te organiseren, afhankelijkheden effectief te beheren en bouwprocessen te vergemakkelijken. Het is fundamenteel voor het creëren van schaalbare en onderhoudbare software in Go.

## Hoe:

Zorg er eerst voor dat je Go geïnstalleerd hebt door `go version` in je terminal te draaien. Je zou de geïnstalleerde versie van Go als output moeten zien. Ga nu naar je werkruimte om een nieuw project te starten en voer uit:

```shell
mkdir hallo-wereld
cd hallo-wereld
```

Dit creëert een nieuwe map voor je project en verplaatst je ernaartoe. Initialiseer nu de module:

```shell
go mod init example.com/hallo-wereld
```

Vervang `example.com/hallo-wereld` met jouw modulepad. Dit commando maakt een `go.mod` bestand in je map, wat het begin van een nieuwe Go module aangeeft. Zo zou `go.mod` eruit kunnen zien:

```plaintext
module example.com/hallo-wereld

go 1.18
```

`go.mod` houdt de afhankelijkheden van je project bij. Maak nu een `main.go` bestand:

```shell
touch main.go
```

Open `main.go` in je favoriete editor en voeg de volgende code toe om "Hallo, Wereld!" af te drukken:

```go
package main

import "fmt"

func main() {
    fmt.Println("Hallo, Wereld!")
}
```

Om je programma uit te voeren, ga terug naar de terminal en voer uit:

```shell
go run main.go
```

Je zou moeten zien:

```plaintext
Hallo, Wereld!
```

Gefeliciteerd! Je hebt zojuist een nieuw Go project opgestart en je eerste Go programma uitgevoerd.

## Diepere Duik

Het initiatief om modules als de standaard voor afhankelijkhedenbeheer in Go te introduceren was een significante verschuiving in het Go ecosysteem, officieel aangenomen in Go 1.11. Voor modules vertrouwden Go ontwikkelaars op de GOPATH-omgevingsvariabele om afhankelijkheden te beheren, wat minder intuïtief was en vaak leidde tot de beruchte "afhankelijkheidshel."

Modules bieden een ingekapselde manier om projectafhankelijkheden te beheren, versiebeheer uit te voeren, en zijn een stap richting het maken van meer zelfstandige en draagbare Go projecten. Elke module specificeert zijn afhankelijkheden die Go bijhoudt in het `go.mod` bestand, waardoor het beheer van afhankelijkheden in verschillende omgevingen en ontwikkelingsstadia wordt vereenvoudigd.

Het is echter vermeldenswaard dat, hoewel Go modules nu de standaard zijn, sommige oudere projecten nog steeds GOPATH kunnen gebruiken. Voor de meeste nieuwe projecten bieden modules een eenvoudiger en effectiever beheersysteem, maar het begrijpen van GOPATH kan handig zijn voor het onderhouden of bijdragen aan oudere Go codebases.

Wat betreft alternatieven, hoewel Go modules nu de facto standaard zijn, heeft de Go-gemeenschap in het verleden geëxperimenteerd met andere tools voor afhankelijkhedenbeheer zoals `dep`. Deze zijn echter grotendeels vervangen door de officiële moduleondersteuning geïntegreerd in de Go-toolchain.
