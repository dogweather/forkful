---
title:                "Swift: Utdrag av substringer"
simple_title:         "Utdrag av substringer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Utdragsinndeling er en viktig del av Swift programmering. Det lar deg enkelt få tak i deler av en streng og manipulere den slik du vil. Dette er spesielt nyttig når du jobber med store mengder tekst og bare trenger å fokusere på en del av den. 

# Hvordan

For å begynne å utvinne delstrenger i Swift, må du først ha en streng å jobbe med. La oss si at vi har en streng som inneholder informasjon om en persons navn og alder: 

```Swift 
let streng = "Eirik, 25" 
``` 

Hvis vi vil skille ut alderen og bruke den til å gjøre noe annet, kan vi bruke følgende kode: 

```Swift 
let delstreng = streng.suffix(2) 
print("Eirik er \(delstreng) år gammel") 
``` 

Dette vil gi følgende utdata: 

`Eirik er 25 år gammel` 

Vi brukte "suffix" funksjonen for å få de to siste tegnene i strengen. Men vi kan også bruke "prefix" funksjonen for å få de to første tegnene: 

```Swift 
let delstreng = streng.prefix(5) 
print("Eiriks navn er \(delstreng)") 
``` 

Dette vil gi følgende utdata: 

`Eiriks navn er Eirik` 

Du kan også bruke "dropFirst" og "dropLast" funksjonene for å ekskludere en viss mengde tegn fra begynnelsen eller slutten av en streng. Her er et eksempel med "dropFirst": 

```Swift 
let delstreng = streng.dropFirst(7) 
print("Alder: \(delstreng)") 
``` 

Dette vil gi følgende utdata: 

`Alder: 25` 

# Dypdykk 

Når du jobber med mer komplekse strenger, kan du også bruke "index" funksjonen for å få tilgang til et bestemt tegn i strengen ved hjelp av et tall. For eksempel hvis vi har en streng med flere ord og vi vil ha det tredje ordet: 

```Swift 
let streng = "Dette er en test" 
let delstreng = streng[streng.index(streng.startIndex, offsetBy: 8)...streng.index(streng.endIndex, offsetBy: -1)] 
print("Tredje ord: \(delstreng)") 
``` 

Dette vil gi følgende utdata: 

`Tredje ord: en` 

Det er også mulig å bruke "range" for å få ut en del av en streng basert på en bestemt posisjon eller indeks. For eksempel: 

```Swift 
let range = streng.index(streng.startIndex, offsetBy: 5)...streng.index(streng.startIndex, offsetBy: 9) 
let delstreng = streng[range] 
print("Delstreng: \(delstreng)") 
``` 

Dette vil gi følgende utdata: 

`Delstreng: er en` 

# Se Også 

- [Swift strings grundig guide - apple.com](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Substring dokumentasjon - apple.com](https://developer.apple.com/documentation/swift/substring)
- [String dokumentasjon - apple.com](https://developer.apple.com/documentation/swift/string/)