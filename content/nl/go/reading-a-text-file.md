---
title:                "Een tekstbestand lezen"
date:                  2024-01-28T22:05:21.689503-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand lezen houdt in dat je de gegevens die in het bestand op je schijf zijn opgeslagen, ophaalt. Programmeurs doen dit om logs, configuraties, gebruikersgegevens – noem maar op – te verwerken, omdat daar vaak de actie zit: de gegevens.

## Hoe:

Een bestand lezen in Go is eenvoudig. Gebruik het `ioutil`-pakket voor een snelle oplossing, of ga voor `os` en `bufio` voor meer controle. Hier is de `ioutil`-manier, makkelijk zat:

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    data, err := ioutil.ReadFile("example.txt")
    if err != nil {
        panic(err)
    }
    fmt.Println(string(data))
}
```

Voor meer finesse, laten we hands-on gaan met `os` en `bufio`:

```Go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }
}
```

In beide gevallen, vervang "example.txt" door de naam van jouw bestand. Voer de code uit, en het zal de inhoud van het bestand uitspugen.

## Diepere Duik

Oorspronkelijk was Go's `ioutil.ReadFile` de go-to methode voor snelle bestandslezingen. Het is een one-liner, maar het leest het hele bestand in één keer. Niet ideaal voor enorme tekstbestanden waar geheugen een zorg is.

Enter `os` en `bufio`. Zij stellen je in staat om het bestand te streamen, regel voor regel te verwerken. Dit betekent dat je gigabytes kunt verwerken zonder te zweten (of je app te breken).

Alternatieven? Zeker. Er zijn pakketten zoals `afero` voor een consistente bestandssysteeminterface, wat handig kan zijn voor testen.

Een beetje implementatiedetail: `bufio.Scanner` heeft een standaard maximale token-grootte (meestal een regel), dus super lange regels kunnen speciale behandeling nodig hebben. Stel dit af met `scanner.Buffer()` als je op dit randgeval stuit.

## Zie Ook

- Om in de details te duiken, bekijk de Go [pakketdocumentatie voor ioutil](https://pkg.go.dev/io/ioutil), [os](https://pkg.go.dev/os), en [bufio](https://pkg.go.dev/bufio).
- Nieuwsgierig naar `afero`? Hier is het [GitHub-repo](https://github.com/spf13/afero).
