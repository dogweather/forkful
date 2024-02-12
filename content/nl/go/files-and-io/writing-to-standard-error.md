---
title:                "Schrijven naar standaardfout"
date:                  2024-02-03T18:15:20.565363-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) in Go houdt in dat foutmeldingen of diagnostische berichten niet bestemd voor de hoofduitvoerstroom worden omgeleid. Programmeurs gebruiken dit om reguliere uitvoer te scheiden van foutinformatie, waardoor debugging en log parsing eenvoudiger wordt.

## Hoe:

In Go biedt het `os`-pakket de waarde `Stderr`, die het standaardfoutbestand vertegenwoordigt. Je kunt dit gebruiken met de functies `fmt.Fprint`, `fmt.Fprintf`, of `fmt.Fprintln` om naar stderr te schrijven. Hier is een eenvoudig voorbeeld:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Een eenvoudige string naar stderr schrijven
    _, err := fmt.Fprintln(os.Stderr, "Dit is een foutmelding!")
    if err != nil {
        panic(err)
    }

    // Geformatteerde foutmelding met Fprintf
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Proces voltooid met %d fouten.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

Voorbeelduitvoer (naar stderr):
```
Dit is een foutmelding!
Proces voltooid met 4 fouten.
```

Onthoud dat deze berichten niet zullen verschijnen in de reguliere uitvoer (stdout), maar in de foutenstroom, die in de meeste besturingssystemen apart kan worden omgeleid.

## Diepere duik

Het concept van standaardfout is diep geworteld in de Unix-filosofie, die duidelijk onderscheid maakt tussen normale uitvoer en foutmeldingen voor efficiëntere gegevensverwerking en -hantering. In Go wordt deze conventie omarmd door het `os`-pakket, dat directe toegang biedt tot de bestandsbeschrijvers stdin, stdout en stderr.

Hoewel rechtstreeks schrijven naar `os.Stderr` geschikt is voor veel toepassingen, biedt Go ook meer geavanceerde logboekregistratiepakketten zoals `log`, die extra functies bieden zoals timestamping en flexibelere uitvoerconfiguraties (bijv. schrijven naar bestanden). Het gebruik van het `log`-pakket, vooral voor grotere toepassingen of waar uitgebreidere logboekregistratiefuncties nodig zijn, kan een beter alternatief zijn. Het is ook de moeite waard om op te merken dat de benadering van Go voor foutafhandeling, die het teruggeven van fouten uit functies aanmoedigt, de praktijk van het schrijven van foutmeldingen naar stderr aanvult, waardoor meer gedetailleerde controle over foutbeheer en rapportage mogelijk is.

In essentie, terwijl het schrijven naar stderr een fundamentele taak is in veel programmeertalen, bieden de standaardbibliotheek en ontwerpprincipes van Go zowel eenvoudige als geavanceerde paden voor het beheren van foutuitvoer, in lijn met bredere industriële praktijken en tegelijkertijd tegemoetkomend aan Go's specifieke ontwerpethos.
