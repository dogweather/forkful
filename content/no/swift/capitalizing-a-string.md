---
title:    "Swift: Store bokstaver i en streng"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng, eller å gjøre den Første bokstaven stor, kan være nyttig når du ønsker å forbedre utseendet på teksten din. Dette kan være spesielt viktig når du lager titler eller overskrifter, eller når du vil fremheve et visst ord eller begrep.

## Hvordan

Det finnes flere måter å kapitalisere en streng på i Swift, avhengig av dine preferanser og behov. En måte er å bruke den innebygde funksjonen `capitalizedString()`, som tar en streng som parameter og returnerer den samme strengen med den første bokstaven stor.

```Swift
let string = "dette er en test"

let capitalizedString = string.capitalizedString()

print(capitalizedString) 
// Dette er en test
```

En annen måte er å bruke metoden `prefix(1)` for å få tak i den første bokstaven i strengen og deretter konvertere den til en stor bokstav ved hjelp av `uppercased()`, og deretter kombinere den med resten av strengen ved hjelp av `+`-operatøren.

```Swift
let string = "dette er en test"

let firstCharacter = string.prefix(1).uppercased()
let restOfString = string.dropFirst()

let capitalizedString = firstCharacter + restOfString

print(capitalizedString) 
// Dette er en test
```

## Deep Dive

Det er verdt å merke seg at metoden `capitalizedString()` tar hensyn til lokaliseringsinnstillingene på enheten, noe som kan føre til at bokstavene i enkelte språk blir kapitalisert annerledes enn man ville forventet.

Det finnes også flere andre metoder i Swift som kan hjelpe deg med å håndtere store og små bokstaver i en streng. For eksempel, hvis du ønsker å konvertere alle bokstavene i en streng til store bokstaver, kan du bruke metoden `uppercased()`, mens `lowercased()` vil konvertere bokstavene til små.

## Se også

- [Swift String API Reference](https://developer.apple.com/documentation/swift/string)
- [Capitalizing First Letter Of String In Swift](https://stackoverflow.com/questions/26306326/capitalizing-the-first-letter-of-a-string-in-swift)