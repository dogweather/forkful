---
title:    "Swift: Utskrift av feilsøkingsutdata"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange utviklere har sannsynligvis opplevd å måtte feilsøke en kode uten å vite hva som egentlig skjer i bakgrunnen. Derfor kan å skrive ut feilmeldinger ved hjelp av debug-utskrift være et nyttig verktøy for å finne og fikse feil. I denne bloggposten vil jeg vise deg hvordan du kan bruke debug-utskrift i Swift for å gjøre debugging-prosessen mer effektiv.

## Hvordan bruke debug-utskrift i Swift

For å bruke debug-utskrift i Swift, kan du bruke funksjonen "print()" for å skrive ut meldinger til konsollen. La oss se på et enkelt eksempel:

```Swift
let tall = 5
print("Tallet er \(tall)")
```

Her vil konsollen vise meldingen "Tallet er 5". Dette kan være nyttig for å sjekke verdier av variabler og for å se om en bestemt del av koden blir utført som forventet.

Du kan også bruke denne funksjonen til å skrive ut uttrykk og variabler for å få mer detaljert informasjon om hva som skjer i koden din. For eksempel:

```Swift
let tall1 = 10
let tall2 = 5
print("Summen av tall1 og tall2 er \(tall1 + tall2)")
```

Dette vil skrive ut meldingen "Summen av tall1 og tall2 er 15" i konsollen. Ved å skrive ut uttrykket får du et bedre innblikk i hva som skjer under kjøring av koden din.

## En dypere forståelse av debug-utskrift

Et annet nyttig aspekt ved debug-utskrift i Swift er muligheten til å legge til betingelser og begrensninger. Dette kan være spesielt nyttig når du vil sjekke om en bestemt del av koden din blir utført som forventet, eller hvis du vil se nærmere på en variabel ved hjelp av en betingelse.

For eksempel, hvis du vil sjekke om et tall er større enn 10 før du skriver det ut, kan du bruke en if-setning slik:

```Swift
let tall = 7
if tall > 10 {
    print("Tallet er større enn 10")
}
```

Dette vil bare skrive ut meldingen hvis betingelsen er oppfylt.

Du kan også bruke debug-utskrift til å sjekke utførelsesrekkefølgen av koden din. Dette kan være nyttig hvis du har flere funksjoner og vil forsikre deg om at de blir kalt i riktig rekkefølge.

## Se også

* [Official Apple Documentation on Debugging in Swift](https://developer.apple.com/documentation/xctest/debugging)
* [Ray Wenderlich's Tutorial on Debugging in Swift](https://www.raywenderlich.com/7382-getting-started-with-swift-3-0/lessons/40)
* [Hacking with Swift's Guide to Debugging](https://www.hackingwithswift.com/quick-start/swift-programming-language/how-to-debug)