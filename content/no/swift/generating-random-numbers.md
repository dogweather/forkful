---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Genererer tilfeldige tall med Swift
Bli kjent med litt uforutsigbarhet i den forutsigbare verden av programmering.

## Hva & Hvorfor?
Lage tilfeldige tall er å produsere tall som ikke kan forutses bedre enn ved en tilfeldig gjetning. Denne prosessen er kritisk i mange områder av programmering, som spill, kryptering og simulering.

## Hvordan:
La oss lage noen virkelig tilfeldige tall i Swift. Her er en enkel måte å gjøre det på:

```Swift
import Foundation

let random = arc4random_uniform(100)
print(random)
```
Når du kjører denne koden, vil det skrives ut et tilfeldig tall mellom 0 og 99.

Hvis du trenger et tall mellom en bestemt range, kan du gjøre som følger:

```Swift
let range = 10...20
let randomInRange = range.randomElement()
print(randomInRange!)
```
Her vil vi få et tilfeldig tall mellom 10 og 20.

## Dypdykk
Historisk sett har generering av tilfeldige tall vært en utfordring i informatikk. De tidligste programmeringsspråkene tilbudt ingen innebygde metoder for dette, så utviklere måtte lage sine egne med varierende resultater.

Alternativer for å generere tilfeldige tall i Swift inkluderer bruken av `arc4random_uniform()` eller `nextInt()`, eller bruk av standard bibliotekfunksjoner som `random()`.

Implementeringsdetaljer kan være komplekse, da de involverer forståelse av statistikker og algoritmer. Imidlertid gjør Swift det enklere for oss ved å skjule grunnleggende detaljer og gir oss enkle funksjoner å bruke.

## Se Også
Du vil kanskje finne disse lenkene nyttige:
2. [Stack Overflow Swift Random Discussion](https://stackoverflow.com/questions/24007461/how-to-enumerate-an-enum-with-string-type)
   
Fortsett å kode, ha det gøy, og la det være litt plass for tilfeldighetene!