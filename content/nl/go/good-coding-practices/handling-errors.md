---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:27.696470-07:00
description: "Foutafhandeling in Go houdt in het herkennen en reageren op foutcondities\
  \ in je programma. Programmeurs doen aan foutafhandeling om ervoor te zorgen dat\u2026"
lastmod: '2024-03-11T00:14:24.103290-06:00'
model: gpt-4-0125-preview
summary: "Foutafhandeling in Go houdt in het herkennen en reageren op foutcondities\
  \ in je programma. Programmeurs doen aan foutafhandeling om ervoor te zorgen dat\u2026"
title: Fouten afhandelen
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling in Go houdt in het herkennen en reageren op foutcondities in je programma. Programmeurs doen aan foutafhandeling om ervoor te zorgen dat hun applicaties zich sierlijk kunnen herstellen van onverwachte situaties, wat leidt tot robuustere en betrouwbaardere software.

## Hoe:

In Go wordt foutafhandeling expliciet beheerd met behulp van het `error` type. Functies die kunnen falen, geven een fout terug als hun laatste retourwaarde. Controleren of deze foutwaarde `nil` is, zal je vertellen of er een fout is opgetreden.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("waarde moet 100 of minder zijn")
    }
    return value * 2, nil
}

func main() {
    resultaat, err := Compute(150)
    if err != nil {
        fmt.Println("Fout:", err)
    } else {
        fmt.Println("Resultaat:", resultaat)
    }
    
    // Een fout sierlijk afhandelen
    eenAnderResultaat, eenAndereFout := Compute(50)
    if eenAndereFout != nil {
        fmt.Println("Fout:", eenAndereFout)
    } else {
        fmt.Println("Resultaat:", eenAnderResultaat)
    }
}
```

Voorbeelduitvoer voor de bovenstaande code:
```
Fout: waarde moet 100 of minder zijn
Resultaat: 100
```

In dit voorbeeld geeft de `Compute` functie ofwel een berekende waarde ofwel een fout terug. De aanroeper handelt de fout af door te controleren of `err` niet `nil` is.

## Diepere Duik

De aanpak van Go voor foutafhandeling is bewust eenvoudig en type-veilig, en vereist expliciete controle op fouten. Dit concept staat in contrast met op uitzonderingen gebaseerde foutafhandeling, zoals gezien in talen zoals Java en Python, waar fouten worden doorgegeven langs de oproepstack tenzij gevangen door een uitzonderingshandler. Het Go-team betoogt dat het expliciet afhandelen van fouten leidt tot duidelijkere en betrouwbaardere code, omdat het programmeurs dwingt om fouten direct aan te pakken waar ze voorkomen.

Echter, sommige kritieken vermelden dat dit patroon kan leiden tot uitgebreide code, vooral in complexe functies met veel foutgevoelige bewerkingen. Als reactie hierop hebben nieuwere versies van Go geavanceerdere foutafhandelingsfuncties geïntroduceerd, zoals foutomwikkeling, waardoor het gemakkelijker wordt om context aan een fout te bieden zonder de oorspronkelijke foutinformatie te verliezen. De gemeenschap heeft ook voorstellen gezien voor nieuwe foutafhandelingsmechanismen, zoals check/handle, hoewel deze tot op mijn laatste update nog onder discussie zijn.

De filosofie van foutafhandeling in Go benadrukt het begrijpen en plannen van fouten als onderdeel van de normale stroom van het programma. Deze aanpak moedigt de ontwikkeling aan van veerkrachtigere en voorspelbaardere software, zij het met een potentiële toename in standaardcode. Alternatieve patronen en bibliotheken bestaan om foutafhandeling te stroomlijnen voor met name complexe gevallen, maar Go's ingebouwde `error` type blijft de basis van foutafhandeling in de taal.
