---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:48.743597-07:00
description: "Het verkrijgen van de huidige datum in Go is een fundamentele taak voor\
  \ programmeurs, vergelijkbaar met \"Hallo, wereld!\" in zijn alomtegenwoordigheid.\
  \ Het\u2026"
lastmod: '2024-03-13T22:44:50.298377-06:00'
model: gpt-4-0125-preview
summary: "Het verkrijgen van de huidige datum in Go is een fundamentele taak voor\
  \ programmeurs, vergelijkbaar met \"Hallo, wereld!\" in zijn alomtegenwoordigheid.\
  \ Het\u2026"
title: De huidige datum krijgen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verkrijgen van de huidige datum in Go is een fundamentele taak voor programmeurs, vergelijkbaar met "Hallo, wereld!" in zijn alomtegenwoordigheid. Het is essentieel voor taken variërend van loggen en tijdstempels op gebeurtenissen zetten tot het berekenen van duur en het plannen van toekomstige gebeurtenissen.

## Hoe te:

In Go is het `time` pakket je toegangspoort tot het werken met datums en tijden. De `time.Now()` functie geeft je de huidige datum en tijd, terwijl andere functies en methoden je toestaan deze gegevens te formatteren of te manipuleren. Hier is hoe je de huidige datum en de verschillende weergaven ervan krijgt:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // Haalt de huidige datum en tijd op
	fmt.Println("Huidige tijd:", currentTime)

	// Om de datum in een JJJJ-MM-DD formaat te krijgen
	fmt.Println("Huidige datum:", currentTime.Format("2006-01-02"))

	// Om de individuele componenten van de datum te krijgen
	jaar, maand, dag := currentTime.Date()
	fmt.Printf("Jaar: %d, Maand: %s, Dag: %d\n", jaar, maand, dag)

	// Om de weekdag te krijgen
	fmt.Println("Weekdag:", currentTime.Weekday())
}
```

Een voorbeelduitvoer kan er zo uitzien:

```
Huidige tijd: 2023-04-18 15:04:05.123456 +0000 UTC
Huidige datum: 2023-04-18
Jaar: 2023, Maand: April, Dag: 18
Weekdag: Dinsdag
```

Merk op hoe `Format` een specifieke datum (2006-01-02) gebruikt als de opmaakstring. Dit is de door Go gekozen referentiedatum, dienend als een ezelsbruggetje voor het formatteren van datums.

## Diepe Duik

De beslissing om het `time` pakket te gebruiken voor datum- en tijdsmanipulatie in Go weerspiegelt de toewijding van de taal aan robuuste en intuïtieve standaardbibliotheken. In tegenstelling tot sommige talen die mogelijk meerdere concurrerende bibliotheken of methodologieën hebben voor datummanipulatie, geeft Go prioriteit aan het hebben van een enkele, goed gedocumenteerde standaard.

De eigenaardige keuze van de referentiedatum (`Mon Jan 2 15:04:05 MST 2006`) in de tijdopmaak van Go, hoewel in eerste instantie verwarrend, is eigenlijk een meesterzet in gebruiksvriendelijkheid. Het stelt programmeurs in staat datum- en tijdformaten te vertegenwoordigen met behulp van een op voorbeelden gebaseerde benadering, in plaats van het memoriseren van tokens of symbolen die andere talen zouden kunnen gebruiken.

Dat gezegd hebbende, hoewel het `time` pakket uitgebreide functionaliteit biedt voor de meeste behoeften, kan het omgaan met tijdzones en DST (zomertijd) veranderingen soms nieuwe Go programmeurs in de war brengen. Het is cruciaal om te begrijpen hoe Go locatiespecifieke tijd hanteert om veelvoorkomende valkuilen in tijdmanipulatie te vermijden.

Voor meer complexe plannings- of tijdmanipulatiebehoeften kunnen externe bibliotheken zoals `github.com/robfig/cron` voor Go gespecialiseerdere functionaliteit bieden dan het standaard `time` pakket. Echter, voor de meeste toepassingen die het verkrijgen en hanteren van de huidige datum en tijd vereisen, biedt het `time` pakket een solide en idiomatische uitgangspunt in Go.
