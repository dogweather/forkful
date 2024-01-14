---
title:                "Swift: Utskrift av feilsøkingsresultater"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å printe ut debug-utdata i Swift kan være en nyttig måte å feilsøke og forstå koden din på. Ved å printe ut verdier og beskrivelser i konsollen, kan du få en bedre forståelse av hva som skjer bak kulissene og hvordan din kode fungerer.

# Hvordan

For å printe ut debug-utdata i Swift, kan du bruke `print()` funksjonen. Denne funksjonen tar inn en hvilken som helst variabel eller uttrykk og vil skrive ut verdien eller beskrivelsen av den i konsollen.

```Swift
let navn = "Sara"
let alder = 25
print("Hei, mitt navn er \(navn) og jeg er \(alder) år gammel.")
// Output: Hei, mitt navn er Sara og jeg er 25 år gammel.
```

For å printe ut verdien av en variabel eller utrykk, kan du også bruke `debugPrint()` funksjonen. Denne funksjonen vil skrive ut en mer detaljert beskrivelse av verdien, inkludert navnet på variabelen eller uttrykket.

```Swift
let tall = 42
debugPrint(tall)
// Output: "tall: 42"
```

Du kan også bruke `dump()` funksjonen for å printe ut hele strukturen av en variabel eller et uttrykk, med alle dets underliggende egenskaper og verdier.

```Swift
struct Person {
    var navn = "Sara"
    var alder = 25
}

let person = Person()
dump(person)
// Output:
// - navn: "Sara"
// - alder: 25
```

# Deep Dive

Når du printer ut debug-utdata, er det viktig å være oppmerksom på hva du ønsker å få ut av det. Hvis du ønsker å se verdien av en variabel, kan du bruke `print()` eller `debugPrint()`, men dersom du ønsker en mer detaljert beskrivelse, kan `dump()` være et bedre alternativ.

Det kan også være lurt å bruke debug-utdata i kombinasjon med breakpoint. Dette gjør det mulig for deg å stoppe koden din på et bestemt sted og se verdien av variabler eller uttrykk på det aktuelle punktet i programmet.

Det finnes også mange verktøy og biblioteker som kan hjelpe deg med å printe ut mer kompleks eller detaljert debug-utdata, inkludert `os_log` og `SwiftLogging`.

# Se også

- [Apple Developer Documentation: Print debugging output](https://developer.apple.com/documentation/swift/debugging_errors_and_logging/printing_debugging_output)
- [Swift By Sundell: Logging in Swift](https://www.swiftbysundell.com/articles/logging-in-swift/)