---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:06.674311-07:00
description: "Hoe te: In Go kan loggen worden ge\xEFmplementeerd met behulp van het\
  \ standaardbibliotheekpakket `log`. Dit pakket biedt eenvoudige logmogelijkheden,\
  \ zoals\u2026"
lastmod: '2024-03-13T22:44:50.294374-06:00'
model: gpt-4-0125-preview
summary: "In Go kan loggen worden ge\xEFmplementeerd met behulp van het standaardbibliotheekpakket\
  \ `log`."
title: Loggen
weight: 17
---

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
