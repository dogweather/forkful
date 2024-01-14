---
title:                "Swift: Slette tegn som matcher et mønster"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
I programmering er det ofte nødvendig å manipulere tekst på ulike måter. En vanlig oppgave kan være å fjerne visse tegn som følger et mønster. Dette kan være nyttig når man for eksempel ønsker å rengjøre data eller formatere tekst på en spesifikk måte. Å slette tegn som matcher et mønster kan også være nyttig når man jobber med brukerinput eller data fra ulike kilder. 

## Hvordan gjøre det
Å slette tegn som følger et bestemt mønster innebærer å bruke visse funksjoner og metoder i Swift. La oss si vi ønsker å fjerne alle vokaler fra en streng. Da kan vi bruke følgende kode:

```Swift
let word = "Programmering"
let vowels = CharacterSet(charactersIn: "aeiouAEIOU")
let result = word.components(separatedBy: vowels).joined()
print(result) // Prgrmmrng
```

Denne koden deler strengen "Programmering" inn i en rekke mindre strenger basert på vokalene, og deretter slår de sammen igjen for å danne den endelige resultatstrengen uten vokaler. Dette er bare ett eksempel, og det finnes mange ulike måter å slette tegn som følger et mønster på i Swift, avhengig av hva slags operasjon du ønsker å gjøre.

## Dypdykk
Hvis du ønsker å lære mer om å slette tegn som matcher et mønster i Swift, kan du lese mer detaljert om de ulike metodene og funksjonene som kan brukes. Det finnes også ulike tredjepartsbiblioteker som kan hjelpe deg med dette, så det kan være lurt å undersøke hva som finnes på markedet. Det er også viktig å være klar over at sletting av tegn som følger et mønster kan være en kompleks oppgave, spesielt hvis du jobber med tekst på flere språk eller med spesielle tegn.

## Se også
- [Swift documentation on CharacterSet](https://developer.apple.com/documentation/foundation/characterset)
- [List of popular Swift libraries](https://www.slant.co/topics/1797/~best-swift-libraries)