---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente nåværende dato innebærer å få en datapunkt som representerer det eksakte øyeblikket i tid da koden kjørte. Dette er viktig fordi koder trenger dette for å spore tidsavhengige data, generere tidspunkter og mer.

## Hvordan:

Her er en grunnleggende måte å hente nåværende dato i Swift:

```Swift 
let nåværendeDato = Date()
print(nåværendeDato)
```

Dette vil gi output som viser den nøyaktige datoen og tiden når koden ble utført. Du vil få noe lignende dette:

```Swift
2021-12-07 21:07:50 +0000
```

## Deep Dive:

Å få tak i nåværende dato er et konsept som programmerere bruker ganske mye. Dato- og tidsfunksjoner som `Date()` er blitt mer og mer sofistikerte gjennom årene for å gi bedre nøyaktighet og effektivitet.

Alternativt, hvis du trenger mer kontroll over tidszoner, kan du bruke `DateFormatter` i Swift. Se eksempel under:

```Swift
let datoFormatter = DateFormatter()
datoFormatter.timeZone = TimeZone(abbreviation: "CET") // Central European Time
datoFormatter.locale = Locale(identifier: "nb_NO") // Norwegian Bokmål
datoFormatter.dateFormat = "y MMM d, HH:mm:ss"
let nåværendeDato = datoFormatter.string(from: Date())
print(nåværendeDato)
```

Denne koden vil gi deg datoen i det spesifikke formatet du har angitt.

Å hente nåværende dato i Swift fungerer ved å lage en instans av `Date`-klassen. Når du oppretter denne instansen uten noen parametere, blir datoen satt til det nøyaktige tidspunktet koden kjøres.

## Se Også:

Hvis du vil dykke dypere inn i Swifts dato- og tidsfunksjoner, her er noen nyttige kilder du kan sjekke ut:

- The Swift Programming Language guide: [Dates and Times](https://developer.apple.com/documentation/foundation/date)
- Swift Date Class Reference: [Date](https://developer.apple.com/documentation/foundation/date)
- DateFormatter Class Reference: [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)