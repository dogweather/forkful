---
title:                "Å jobbe med yaml"
html_title:           "Arduino: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML (YAML Ain't Markup Language) er et enkelt og intuitivt tekstformat som brukes til å konfigurere og organisere data. For Arduino-programmering, kan YAML veies som et alternativ til JSON-formatet for å lagre og strukturere data. Dette kan være nyttig for å skrive mer lesbar og vedlikeholdbar kode.

## Hvordan

For å bruke YAML i din Arduino-kode, må du først inkludere en YAML-parser biblioteket i ditt prosjekt. Dette gjøres ved å velge "Library Manager" i verktøy-menyen i Arduino-programmet, og søke etter "YAML". Installer det tilgjengelige biblioteket og inkluder det i koden din ved å skrive `#include <YAML.h>` øverst.

For å arbeide med YAML data, må du først initialisere et `YAML::Node` objekt. Dette vil være rot-noden for dine data, og du kan legge til undernoder og nøkkel-verdi-par under den. For eksempel:

```arduino
YAML::Node minNode; // Initialiserer en tom YAML-node

minNode["navn"] = "Arduino"; // Legger til en nøkkel-verdi-par
minNode["pin"] = 13;

YAML::Node subNode; // Initialiserer en undernode
subNode["verdi"] = 100;
minNode["data"] = subNode; // Legger til undernoden i hovednoden
```

For å skrive ut innholdet av YAML-noden, kan du bruke `Serial.println()` -funksjonen og sende noden som en streng:

```arduino
Serial.println(minNode ? YAML::Dump(minNode).c_str()); // Skriver ut YAML-noden som en streng
```

Dette vil produsere følgende output i serieovervåkingsvinduet: 

```
navn: Arduino
pin: 13
data:
    verdi: 100
```

For å lese og behandle YAML-data som er mottatt fra en annen enhet, kan du bruke `YAML::Load()` -funksjonen. Denne funksjonen tar en streng av YAML-data og konverterer den til en `YAML::Node` som du kan jobbe med.

```arduino
String mottattData = "navn: Arduino\npin: 13\ndata:\n    verdi: 100"; // Simulerer mottatt YAML-data
YAML::Node lesNode = YAML::Load(mottattData); // Konverterer dataen til en YAML-node

// Hente ut og bruke data fra noden
String enhetsnavn = lesNode["navn"].as<String>();
int pinNummer = lesNode["pin"].as<int>();
int sensorVerdi = lesNode["data"]["verdi"].as<int>();
```

## Dykk dypere

Det er verdt å merke seg at YAML, i motsetning til JSON, støtter å inkludere kommentarer og har en mer naturlig og lesbar syntax. Dette gjør det til et godt valg for å konfigurere og organisere komplekse datastrukturer for Arduino-prosjekter.

En annen interessant funksjon ved YAML er muligheten til å referere til og gjenbruke data fra andre deler av YAML-filen. Dette kan være nyttig hvis du har noen nøkkel-verdi-par som blir gjentatt flere ganger. For å gjøre dette, kan du bruke et ampersand-tegn (`&`) etterfulgt av en unik etikett, og et stjerne-tegn (`*`) etterfulgt av samme etikett når du vil referere til dataen.

```yaml
tilbehor: &utstyr # Referer til denne listen med data
    navn: Fotball
    farge: Hvit og svart
    pris: 200
gerilja_markering:
    farge: Grønn
    pris: *utstyr.pris # Refererer til prisen på fotball for denne typen utstyr
arv_markering:
    farge: Blå
    pris: *utstyr.pris # Refererer til prisen på fotball for denne typen utstyr
```

Denne funksjonen kan hjelpe til med å unngå duplisert kode