---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:51.130054-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera textuella\
  \ datum- och tidsrepresentationer till ett `Date`-objekt. Denna process \xE4r avg\xF6\
  rande i\u2026"
lastmod: '2024-03-13T22:44:38.260308-06:00'
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera textuella\
  \ datum- och tidsrepresentationer till ett `Date`-objekt. Denna process \xE4r avg\xF6\
  rande i\u2026"
title: "Analysera ett datum fr\xE5n en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att konvertera textuella datum- och tidsrepresentationer till ett `Date`-objekt. Denna process är avgörande i applikationer där datum kommuniceras som strängar, till exempel i API-svar eller användarinmatningar, vilket möjliggör enklare hantering och formatering av datum.

## Hur man gör:

### Använda Foundations `DateFormatter`
Swifts standardbibliotek, Foundation, tillhandahåller `DateFormatter` för att konvertera strängar till `Date`-objekt och vice versa. För att tolka ett datum från en sträng specificerar du det datumformat som matchar strängen, sedan använder du formateraren för att tolka den.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("Tolkat datum: \(date)")
} else {
    print("Misslyckades med att tolka datum")
}
// Exempelutskrift: Tolkat datum: 2023-04-29 22:00:00 +0000
```

Notera att utskriften kan variera baserat på din tidszon.

### Använda ISO8601DateFormatter
För ISO 8601-datumformat erbjuder Swift en specialiserad formaterare, `ISO8601DateFormatter`, som förenklar tolkningsprocessen.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("Tolkat ISO8601-datum: \(date)")
} else {
    print("Misslyckades med att tolka ISO8601-datum")
}
// Exempelutskrift: Tolkat ISO8601-datum: 2023-04-30 15:00:00 +0000
```

### Använda ett tredjepartsbibliotek: SwiftDate
Även om Swift tillhandahåller robusta verktyg för datumtolkning, erbjuder tredjepartsbibliotek som SwiftDate ännu större flexibilitet och bekvämlighet. Efter att ha lagt till SwiftDate i ditt projekt blir tolkningen så enkel som:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("Tolkat datum med SwiftDate: \(date)")
} else {
    print("Misslyckades med att tolka datum med SwiftDate")
}
// Exempelutskrift: Tolkat datum med SwiftDate: 2023-04-30 00:00:00 +0000
```

SwiftDate förenklar tolkning med naturligt språk och ett brett utbud av datumformat, vilket gör det till ett kraftfullt tillskott till ditt Swift-programmeringsverktygslåda.
