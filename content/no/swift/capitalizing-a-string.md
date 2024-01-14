---
title:                "Swift: Stor bokstaving av en streng"
simple_title:         "Stor bokstaving av en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Det kan være flere grunner til å ville kapitalisere en streng i Swift. Det kan være for å gjøre teksten mer lesbar for brukere, for å følge konvensjoner i språket eller for å tilpasse den visuelle utformingen av teksten.

# Hvordan

For å kapitalisere en streng i Swift kan du bruke funksjonen `uppercased()`. Denne funksjonen tar en streng som parameter og returnerer en ny streng med alle bokstavene i kapital bokstaver.

```Swift
let navn = "johan"
let kapitalisertNavn = navn.uppercased()
print(kapitalisertNavn) // Output: JOHAN
```

I dette eksempelet har vi definert en streng kalt "navn" med navnet "johan". Deretter bruker vi funksjonen `uppercased()` på strengen og lagrer den i en ny konstant kalt "kapitalisertNavn". Til slutt skriver vi ut den kapitaliserte strengen og får "JOHAN" som output.

# Dypdykk

Ved å bruke funksjonen `uppercased()` vil alle bokstavene i strengen bli konvertert til store bokstaver. Dette inkluderer også bokstaver med aksenter eller diakritiske tegn, som æ, ø og å. Det finnes også en tilsvarende funksjon som heter `capitalized()`, som vil kun kapitalisere den første bokstaven i strengen, og gjøre resten av bokstavene til små bokstaver.

Det er også verdt å merke seg at disse funksjonene kun vil fungere på engelskspråklige bokstaver. Dersom du ønsker å kapitalisere bokstaver fra andre språk, må du bruke en annen metode som tar hensyn til disse bokstavene.

# Se også

- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Upper and Lowercase Strings in Swift](https://learnappmaking.com/uppercased-lowercased-capitalized-swift-string-how-to/)