---
title:    "Swift: Konvertering av en streng til små bokstaver."
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger i programmering er det nødvendig å konvertere en streng til små bokstaver for å kunne behandle data på en mer effektiv måte. Dette kan for eksempel være når man arbeider med brukerinput, eller når man skal sammenligne to strenger for å se om de er like uavhengig av store eller små bokstaver.

## Slik gjør du det
For å konvertere en streng til små bokstaver i Swift, kan du bruke funksjonen ```lowercased()```. Denne funksjonen tar inn en streng og returnerer en ny streng med kun små bokstaver. Se eksempelet under:

```Swift
let navn = "Maria"
let navnLiten = navn.lowercased()
print(navnLiten)
```
Output:
```
maria
```

## Dypdykk
Det kan være lurt å tenke på hvilket språk og alfabet du arbeider med når du jobber med å konvertere strenger til små bokstaver. For eksempel vil en kinesisk tekst se annerledes ut enn en norsk tekst når den er konvertert til små bokstaver, fordi kinesisk ikke har store og små bokstaver slik vi kjenner det.

Hvis du ønsker å behandle kun én bokstav om gangen, kan du bruke funksjonen ```unicodeScalars```. Da kan du gå gjennom hver bokstav i strengen og konvertere den til små bokstaver ved hjelp av funksjonen ```uppercased()```.

## Se også
- [Apple Swift Docs: Using Swift with Cocoa and Objective-C](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/BuildingCocoaApps/index.html)
- [Swift Programming Tutorials: Strings and Characters](https://www.tutorialspoint.com/swift/swift_strings.htm)
- [Ray Wenderlich: Swift String Cheat Sheet](https://www.raywenderlich.com/965-swift-string-cheat-sheet-and-quick-reference/attachments/104142)