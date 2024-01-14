---
title:    "Swift: Å få dagens dato"
keywords: ["Swift"]
---

{{< edit_this_page >}}

Hvorfor: Å få den nåværende datoen i et program kan være svært nyttig for å holde styr på tidsbaserte funksjoner eller for å gi brukeren relevant informasjon, for eksempel når en bestemt handling ble utført.

Hvordan: For å få den nåværende datoen i Swift, kan man bruke Date-klassen og dens metoder. Først må man importere Foundation-biblioteket. Deretter kan man opprette et Date-objekt ved å bruke konstruktøren "Date()". For å få datoen på ønsket format, kan man bruke DateFormatter-klassen. Nedenfor er et eksempel på hvordan kodeblokker kan se ut i Swift:

```Swift
import Foundation
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate)
```

Dette vil gi følgende output:

```
23.05.2021
```

Dykk ned: Det er også mulig å få mer spesifikk informasjon om den nåværende datoen ved å bruke DateComponents-klassen. Denne klassen lar deg få tilgang til felter som år, måned, dag osv. For å bruke denne klassen, må man først opprette et Calendar-objekt og deretter bruke metoden "dateComponents" for å få tilgang til ønskede felter. For eksempel kan man få tilgang til måneden ved å bruke følgende kode:

```Swift
import Foundation
let calendar = Calendar.current
let components = calendar.dateComponents([.month], from: Date())
print(components.month!) // ! er nødvendig for å unwrapped verdien, siden den returnerer en optional 
```

Dette vil gi følgende output:

```
5
```

Se også: Hvis du vil lære mer om hvordan du jobber med datoer i Swift, kan du sjekke ut følgende linker:
- Offisiell Swift dokumentasjon for Date-klassen: https://developer.apple.com/documentation/foundation/date
- Tutorial om hvordan du bruker Date-klassen: https://www.raywenderlich.com/842-swift-tutorial-part-1-expressions-variables-and-constants
- Dokumentasjon om DateFormatter-klassen: https://developer.apple.com/documentation/foundation/dateformatter
- Tutorial om hvordan du bruker DateFormatter-klassen: https://learnappmaking.com/nsdate-date-swift-how-to/