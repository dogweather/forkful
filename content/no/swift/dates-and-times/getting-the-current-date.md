---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:06.986361-07:00
description: "\xC5 hente den n\xE5v\xE6rende datoen i Swift inneb\xE6rer \xE5 bruke\
  \ `Date`-klassen for \xE5 f\xE5 tilgang til datoen og tiden appen blir kj\xF8rt.\
  \ Programmerere trenger \xE5 hente\u2026"
lastmod: '2024-03-13T22:44:41.152606-06:00'
model: gpt-4-0125-preview
summary: "\xC5 hente den n\xE5v\xE6rende datoen i Swift inneb\xE6rer \xE5 bruke `Date`-klassen\
  \ for \xE5 f\xE5 tilgang til datoen og tiden appen blir kj\xF8rt. Programmerere\
  \ trenger \xE5 hente\u2026"
title: "F\xE5 dagens dato"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente den nåværende datoen i Swift innebærer å bruke `Date`-klassen for å få tilgang til datoen og tiden appen blir kjørt. Programmerere trenger å hente den nåværende datoen av mange grunner, som strekker seg fra å tidsstemple hendelser, utføre datoberegninger, til å vise datoer og tider i et brukergrensesnitt.

## Hvordan:
Swifts `Foundation`-rammeverk tilbyr `Date`-klassen, noe som gjør det enkelt å få den nåværende datoen og tiden. Her er et grunnleggende eksempel på hvordan du får den nåværende datoen:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Dette vil gi en utskrift som ligner på:

```
2023-04-12 07:46:23 +0000
```

Utskriftsformatet følger ISO 8601-standarden, og bruker UTC-tidssonen. Imidlertid ønsker du kanskje å formatere denne datoen for visningsformål. Swifts `DateFormatter`-klasse kommer til unnsetning:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Et eksempel på utskrift kan være:

```
12. april 2023 kl. 10:46:23
```

Merk at utskriftsformatet vil variere avhengig av enhetens lokalitet som kjører koden.

For prosjekter som krever mer kompleks datohåndtering, vender mange Swift-utviklere seg til tredjepartsbiblioteker som `SwiftDate`. Her er hvordan du kan bruke `SwiftDate` for å få den nåværende datoen i en spesifikk tidssone og format:

Først, legg til `SwiftDate` i prosjektet ditt ved hjelp av SPM, CocoaPods, eller Carthage. Deretter:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Dette kan gi en utskrift på:

```
2023-04-12 09:46:23
```

Ved å bruke `SwiftDate`, kan du enkelt manipulere datoer og tider for ulike tidssoner og lokaliteter, noe som forenkler komplekse dato-håndteringsoppgaver i Swift-applikasjonene dine.
