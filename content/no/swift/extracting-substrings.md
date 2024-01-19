---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente ut substrings er prosessen å isolere en del av en streng i Swift. Programmerere gjør dette for å analysere og manipulere data på mer effektive måter.

## Hvordan:

Her er noen kjerneeksempler på hvordan å hente ut substrings i Swift:

```Swift
let tekst = "Swift Programmering"
let startIndex = tekst.index(tekst.startIndex, offsetBy: 6)
let endIndex = tekst.index(tekst.startIndex, offsetBy: 19)
let substring = tekst[startIndex..<endIndex]

print(substring) 
// Output: Programmering
```
I dette eksemplet, spesifiserer vi start og slutt indekser for substringen vi vil ha, og Swift gir oss dataene i de gitte grensene.

## Dybde Dykk

1. **Historisk Kontekst:** Metoden for å hente ut substringer i Swift har blitt mer forenklet og intuitivt over tid. I tidligere Swift-versjoner skulle du slite mer med å håndtere denne oppgaven.

2. **Alternativer:** Du kan også bruke `prefix(_:)` og `suffix(_:)` metoder for å hente ut substringer fra begynnelsen eller slutten av en streng.

```Swift
let forsteFem = tekst.prefix(5)
print(forsteFem) 
// Output: Swift

let sisteTre = tekst.suffix(3)
print(sisteTre) 
// Output: ing
```

3. **Implementasjonsdetaljer:** Når du bruker disse metodene, returnerer Swift en instans av `Substring` typen, ikke en ny `String`. Dette er for å spare minne, og det er oftest en øyeblikksoperasjon.

## Se også:

For mer informasjon og øvelser på substrings i Swift, kan du lese:

1. Swift dokumentasjon på substrings: [https://docs.apple.com/swift/standard-library/SubString](https://docs.apple.com/swift/standard-library/SubString) 
2. Bloggpost på "Working with Strings in Swift": [https://www.hackingwithswift.com/articles/141/working-with-strings-in-swift](https://www.hackingwithswift.com/articles/141/working-with-strings-in-swift)