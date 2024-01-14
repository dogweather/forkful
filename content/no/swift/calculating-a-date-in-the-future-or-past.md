---
title:    "Swift: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor
Å beregne en dato i fremtiden eller fortiden kan være nyttig i mange programmeringsprosjekter. For eksempel kan det være nyttig å vise en fremtidig dato for en planlagt hendelse, eller å beregne alderen til en person ved en bestemt dato.

## Slik gjør du det
For å kunne beregne en dato i fremtiden eller fortiden, trenger du å bruke Swift's `Calendar` og `DateComponents`-klasser. Først må du opprette en `Date`-variabel med dagens dato. Deretter kan du bruke `Calendar`-klassen til å legge til eller trekke fra et ønsket antall dager, uker, måneder eller år til / fra datoen. Til slutt kan du bruke `DateFormatter` for å formatere datoen etter ønsket format.

```Swift
let currentDate = Date()

// For å legge til 3 dager til datoen:
let futureDate = Calendar.current.date(byAdding: .day, value: 3, to: currentDate)

// For å trekke fra 2 uker fra datoen:
let pastDate = Calendar.current.date(byAdding: .week, value: -2, to: currentDate)

// For å vise datoen med formatet "dd/MM/yyyy":
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let formattedDate = dateFormatter.string(from: futureDate)
```

## Dykk dypere
Dette er bare et grunnleggende eksempel på hvordan du kan beregne en dato i Swift. Det finnes også andre metoder for dette, som bruk av `DateComponents` direkte eller å bruke tidsverdier som sekunder for å legge til og trekke fra tid. Det er viktig å ha en god forståelse av Swift's dato og tid-klasser og hva de kan gjøre for å kunne utføre nøyaktige beregninger.

## Se også
- [Swift's Dato og tid-dokumentasjon](https://developer.apple.com/documentation/foundation/date)
- [Using Date and Time in Swift](https://learnappmaking.com/date-nsdate-nstimeinterval-swift-how-to/)