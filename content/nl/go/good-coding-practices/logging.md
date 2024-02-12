---
title:                "Loggen"
aliases: - /nl/go/logging.md
date:                  2024-02-03T17:59:06.674311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loggen"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Loggen in softwareontwikkeling is het proces van het vastleggen van informatie over de uitvoering van een programma, ontworpen om zijn gedrag te volgen en problemen te diagnosticeren. Programmeurs implementeren loggen om softwareprestaties te monitoren, fouten te debuggen en de systeemveiligheid en -compliance te waarborgen, waardoor het een onmisbaar hulpmiddel wordt voor applicatieonderhoud en -analyse.

## Hoe te:

In Go kan loggen worden geïmplementeerd met behulp van het standaardbibliotheekpakket `log`. Dit pakket biedt eenvoudige logmogelijkheden, zoals schrijven naar standaarduitvoer of naar bestanden. Laten we beginnen met een basisvoorbeeld van loggen naar de standaarduitvoer:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Dit is een basis logboekvermelding.")
}
```

Uitvoer:
```
2009/11/10 23:00:00 Dit is een basis logboekvermelding.
```

De tijdstempel aan het begin van de logvermelding wordt automatisch toegevoegd door het `log`-pakket. Vervolgens laten we zien hoe we in plaats van naar de standaarduitvoer naar een bestand kunnen loggen:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Deze logvermelding gaat naar een bestand.")
}
```

Nu, laten we een meer geavanceerd gebruik implementeren: het aanpassen van het logformaat. Go staat je toe om een aangepaste logger te creëren met `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "AANGEPASTE LOG: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Dit is een aangepast logbericht.")
}
```

Uitvoer:
```
AANGEPASTE LOG: 2009/11/10 23:00:00 main.go:11: Dit is een aangepast logbericht.
```

Dit voorbeeld voorziet elk logbericht van de prefix "AANGEPASTE LOG: " en bevat de datum, tijd en locatie van het bronbestand.

## Dieper ingaan

Het `log`-pakket van de Go-standaardbibliotheek is eenvoudig en voldoende voor veel toepassingen, maar mist enkele van de meer geavanceerde functies die in externe logbibliotheken worden gevonden, zoals gestructureerd loggen, logrotatie en loggen op basis van niveaus. Pakketten zoals `zap` en `logrus` bieden deze geavanceerde functies en worden in de Go-gemeenschap gewaardeerd om hun prestaties en flexibiliteit.

Gestructureerd loggen maakt het bijvoorbeeld mogelijk om gegevens in een gestructureerd formaat (zoals JSON) te loggen, wat vooral nuttig is voor moderne cloud-gebaseerde applicaties waar logs mogelijk worden geanalyseerd door verschillende hulpmiddelen of diensten. `zap` staat in het bijzonder bekend om zijn hoge prestaties en lage allocatieoverhead, waardoor het geschikt is voor applicaties waar snelheid en efficiëntie cruciaal zijn.

Historisch gezien is loggen in Go aanzienlijk geëvolueerd sinds de oprichting van de taal. Eerdere versies van Go boden de basis logmogelijkheden die we zien in het `log`-pakket. Echter, naarmate de taal in populariteit groeide en de complexiteit van in Go geschreven applicaties toenam, begon de gemeenschap geavanceerdere logbibliotheken te ontwikkelen om aan hun behoeften te voldoen. Vandaag de dag, terwijl het standaard `log`-pakket nog steeds een haalbare optie blijft voor eenvoudige applicaties, wenden veel ontwikkelaars zich tot deze externe oplossingen voor complexere logvereisten.
