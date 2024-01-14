---
title:                "Swift: Konvertere en dato til en tekststreng"
simple_title:         "Konvertere en dato til en tekststreng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en vanlig oppgave i mange programmeringsprosjekter. Dette gjøres ofte for å presentere datoen på en mer leselig måte for brukerne eller for å formatere den på en bestemt måte i forhold til ulike lokaliseringsinnstillinger.

## Slik gjør du det

For å konvertere en dato til en streng i Swift, bruker du `DateFormatter` - klassen. Først må du opprette en `Date`-instans som inneholder datoen du vil konvertere.

```Swift
let today = Date()
```

Deretter oppretter du en `DateFormatter`-instans og angir ønsket datoformat.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
```

Til slutt kjører du `string(from:)`-metoden på `DateFormatter`-instansen og passerer `Date`-objektet du ønsker å konvertere som argument.

```Swift
let strDate = dateFormatter.string(from: today)
print(strDate) // 09.08.2021
```

## Dykk dypere

I tillegg til å formatere datoen etter ønsket mønster, kan du også endre datoens stil og språk ved å bruke ulike verdier for `dateStyle` og `locale` egenskapene på `DateFormatter`-instansen.

For eksempel kan du sette datoformatet til kort månednavn og langt årstall på engelsk.

```Swift
dateFormatter.dateStyle = .short
dateFormatter.timeStyle = .none
dateFormatter.locale = Locale(identifier: "en_US")
print(dateFormatter.string(from: today)) // 8/9/21
```

## Se også

- [Apple Dev Documentation on DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift Blog post on Date Formatting](https://swift.org/blog/date-and-time/)