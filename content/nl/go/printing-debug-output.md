---
title:                "Debug-output afdrukken"
date:                  2024-01-28T22:04:41.274008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Debug-uitvoer afdrukken is het uitspuwen van gegevens om te controleren wat je code doet. Programmeurs doen dit om bugs op te sporen of om de stroom en de datastatus in één oogopslag te begrijpen.

## Hoe:
Hier is hoe je enkele printregels in je Go-code kunt invoegen.

```Go
package main

import (
    "fmt"
    "log"
)

func main() {
    // Basis print naar stdout
    fmt.Println("Hallo, ik ben een printopdracht!")

    // Geformatteerde print
    name, age := "Jane", 28
    fmt.Printf("%s is %d jaar oud.\n", name, age)

    // Printen met log (bevat tijdstempel)
    log.Println("Dit is een gelogde informatie met een tijdstempel.")

    // Voor debuggen, gebruik Printf, maar vergeet niet om het later te verwijderen
    debug := true
    if debug {
        fmt.Printf("Debuginfo: %s is %d jaar oud.\n", name, age)
    }
}
```

Voorbeelduitvoer:
```
Hallo, ik ben een printopdracht!
Jane is 28 jaar oud.
2009/11/10 23:00:00 Dit is een gelogde informatie met een tijdstempel.
Debuginfo: Jane is 28 jaar oud.
```

## Diepgaand:
Historisch gezien is `fmt` Go's go-to voor I/O-operaties sinds de oprichting. Het staat voor 'format' en biedt een reeks functies om tekstuitvoer te vormen. `Println` en `Printf` zijn hier belangrijke functies. Het `log`-pakket voegt tijd toe, samenhangend voor het volgen van gebeurtenissen in de tijd.

Alternatieven? Zeker, naast basis printopdrachten, kun je log-frameworks zoals `logrus` of `zap` gebruiken voor gestructureerd en geniveleerd loggen, perfect voor serieuze toepassingen.

Implementatiedetails? `fmt` is thread-safe, waardoor je debugprints vanuit gelijktijdige goroutines begrijpelijk zijn. Maar let op, debugprints zijn goed voor een snelle blik, maar kunnen je vertragen of een rommel maken in productiecode.

## Zie Ook:
- Go by Example over `fmt`: https://gobyexample.com/fmt
- De Go Blog over "Het Gebruiken van Go Modules": https://blog.golang.org/using-go-modules (bekijk het deel over geleverde afhankelijkheden)
- Go Documentatie voor `log`: https://pkg.go.dev/log
- Gestructureerd loggen in Go met `logrus`: https://github.com/sirupsen/logrus
- Razendsnel, gestructureerd, geniveleerd loggen in Go met `zap`: https://github.com/uber-go/zap
