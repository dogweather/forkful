---
title:                "Swift: Henting av nåværende dato"
simple_title:         "Henting av nåværende dato"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Du har kanskje lurt på hvordan man får tak i dagens dato ved programmering i Swift. Dato og tid er essensielle elementer i mange programmer, og å få tilgang til denne informasjonen kan være svært nyttig. I denne bloggposten vil jeg gå gjennom hvordan du enkelt kan få tak i dagens dato ved hjelp av Swift.

## Hvordan

Først må vi importere klassen "Foundation" for å få tilgang til funksjoner som håndterer dato og tid.

```Swift
import Foundation
```

Deretter kan vi bruke funksjonen "Date()" for å få tak i dagens dato.

```Swift
let currentDate = Date()
```

Vi kan også formatere dato og tid ved hjelp av en DateFormatter. Her kan vi strukturere utseendet på dato og tid ved å legge til ønskede symboler mellom klammeparentesene. 

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy HH:mm"
let formattedDate = formatter.string(from: currentDate)

print(formattedDate)
```

Dette vil gi oss en output som ser slik ut: "29.06.2021 14:30". Vi kan selv velge hvilket format vi ønsker å bruke basert på våre behov.

## Deep Dive

Date-klassen har mange nyttige funksjoner som gir oss mer informasjon om dagens dato og tid. For eksempel kan vi få tak i detaljert informasjon om tidssoner ved hjelp av TimeZone-klassen. Vi kan også bruke Calendar-klassen til å få tak i spesifikke datoer og gjøre beregninger med tid.

Et viktig punkt å huske på er at dato og tid kan være forskjellig for ulike land og regioner. Derfor er det viktig å alltid være oppmerksom på hvilken tidssone og kalender man bruker når man jobber med dato i programmering.

## Se også

- [Dokumentasjon](https://developer.apple.com/documentation/foundation/date)
- [Tutorial](https://www.hackingwithswift.com/example-code/system/how-to-get-the-current-date-and-time-using-date)
- [Stack Overflow spørsmål](https://stackoverflow.com/questions/46598623/current-date-and-time-in-swift)