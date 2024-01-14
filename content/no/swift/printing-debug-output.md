---
title:                "Swift: Utskrift av feilsøkingsdata"
simple_title:         "Utskrift av feilsøkingsdata"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang sittet på din beste venns maskin og feilsøkt en Swift-applikasjon du har utviklet sammen? Eller kanskje prøvd å finne ut hvorfor din egen kode ikke fungerer som forventet? Det kan være en frustrerende og tidkrevende prosess å finne ut feilen i koden. Heldigvis er det et enkelt verktøy som kan gjøre denne prosessen mye lettere - print debugging. Ved å skrive ut informasjon om variabler, funksjonskall og fler i koden din, kan du enklere forstå hva som skjer og finne feilen raskere. I denne bloggposten vil jeg vise deg hvordan du kan bruke print debugging i Swift.

## Hvordan

For å skrive ut en variabel i Swift, bruker du print-funksjonen og angir variabelens navn slik:

```Swift
let navn = "Maria"
print(navn)
```

Dette vil skrive ut verdien til variabelen "navn" i konsollen:

``` Swift
Maria
```

Du kan også skrive ut flere variabler ved å separere dem med komma:

```Swift
let alder = 28
let yrke = "programmerer"
print(navn, alder, yrke)
```

Dette vil skrive ut følgende i konsollen:

```
Maria 28 programmerer
```

Du kan også bruke print-funksjonen til å skrive ut informasjon om funksjonskall og fler. La oss si at du har en funksjon som regner ut gjennomsnittet av to tall:

```Swift
func beregnGjennomsnitt(tall1: Double, tall2: Double) -> Double {
    let gjennomsnitt = (tall1 + tall2) / 2
    print("Gjennomsnittet av \(tall1) og \(tall2) er \(gjennomsnitt)")
    return gjennomsnitt
}
```

Når du kaller på denne funksjonen, vil print-funksjonen skrive ut følgende i konsollen:

```Swift
Gjennomsnittet av 4.5 og 7.2 er 5.85
```

Bytte til å bruke print-funksjonen istedenfor å bare vise verdier i en UI-kontroller vil også være nyttig når du trenger å undersøke hva som skjer med verdier når appen kjører i en reell situasjon.

## Dypdykk

En annen nyttig funksjon med print-funksjonen er at du kan bruke den for å debugge komplekse kodesnutter. For å gjøre dette, må du bruke en "kittstruktur" som gjør at du kan bryte ned koden i mindre deler og deretter skrive ut informasjon om disse delene for å forstå hva som skjer. La oss si at du utvikler en app som genererer en QR-kode basert på en tekststreng. Koden ser slik ut:

```Swift
import Foundation
import UIKit

let tekst = "ABC1234"
let qrdata = tekst.data(using: .isoLatin1)
let filter = CIFilter(name: "CIQRCodeGenerator")
filter?.setValue(qrdata, forKey: "inputMessage")
let image = UIImage(ciImage: (filter?.outputImage)!)
```

Hvis du har problemer med å få appen til å generere QR-koden riktig, kan du bruke print-funksjonen til å undersøke hva som skjer med hver av variablene:

```Swift
print(qrdata)
print(filter?.outputImage)
```

Dette vil gi deg verdifull informasjon om hva som skjer med variablene og hjelpe deg å finne feilen.

## Se også

- [The Ultimate Guide to Debugging in Swift](https://learnappmaking.com/debugging-swift-how-to/)
- [Debugging and Printing in Swift](https://medium.com/flawless-app-stories/debugging-and-printing-in-swift-a8a2e8a2293e)
- [Swift Print Statement and Debugging Techniques](https://developer.apple.com/swift/blog/?id=16)