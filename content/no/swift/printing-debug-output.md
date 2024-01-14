---
title:    "Swift: Utskrift av feilsøkingsutdata"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

Våre lesere lurer kanskje på hvorfor det er viktig å inkludere utskrift av feilsøking i sine Swift programmeringsprosjekter. Å legge til debugging output kan hjelpe deg med å identifisere feil og feilkilder, noe som vil gjøre debugging prosessen mye enklere og mer effektiv.

## Hvordan

For å legge til debugging output i Swift, kan du bruke funksjonen `print()`. La oss si du har en variabel som inneholder navnet ditt, og du vil sjekke om variabelen er korrekt tilordnet. Da kan du skrive følgende kode i et `if`-statement:

```Swift
let navn = "Ole"

if navn == "Ole" {
    print("Navnet er korrekt!")
} else {
    print("Navnet er ikke korrekt")
}
```

Når du kjører koden, vil du se at "Navnet er korrekt!" blir utskrevet i konsollen, noe som betyr at variabelen er korrekt tilordnet.

## Dypdykk

Det er viktig å være klar over at debugging output ikke bør inkluderes i den endelige koden din. Det kan påvirke ytelsen til applikasjonen din og gjøre den tregere. Derfor er det viktig å fjerne eller kommentere ut alle debugging statements før du publiserer koden.

I tillegg til `print()` funksjonen, kan du også bruke `debugPrint()` som vil gi deg mer detaljert informasjon om objekter og deres verdier. Du kan også bruke `#file`, `#line` og `#function` for å få informasjon om hvilken fil, linje og funksjon som er ansvarlig for debugging output.

## Se også

* [Apple: Debugging with Print](https://developer.apple.com/documentation/swift/debugging_with_print)
* [Swift by Example: Printing Debug Output](https://www.swiftbyexample.com/print-debug)
* [Hacking with Swift: Debugging 101](https://www.hackingwithswift.com/read/15/5/debugging-101)