---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Swift: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?:
Lesing av kommandolinjeargumenter er en måte for programmerere å gi instruksjoner til et program ved hjelp av tekstkommandoer som er skrevet i terminalen. Dette er nyttig for automatisering av oppgaver og kan gjøre det enklere å kjøre flere kommandoer samtidig.

Slik gjør du:
```Swift
let arguments = CommandLine.arguments
print("Kommando linje argumenter: \(arguments)")
```
Dette kodeeksempelet viser hvordan du kan bruke 'CommandLine' for å lese kommandolinjeargumenter og deretter skrive ut dem. Hvis du for eksempel kjører programmet med argumentene "Hello" og "World", vil konsollen skrive ut "Kommando linje argumenter: [Hello, World]".

Dypdykk:
Historisk kontekst: Lesing av kommandolinjeargumenter har vært en viktig del av programmering siden begynnelsen av datamaskinene. Før grafiske brukergrensesnitt ble vanlige, var det eneste måten å kommunisere med datamaskiner gjennom kommandolinjen.

Alternativer: Selv om lesing av kommandolinjeargumenter er en viktig del av programmering, er det ikke den eneste måten å interagere med et program på. GUI-baserte brukergrensesnitt har blitt vanligere og mer populære blant programmerere.

Implementeringsdetaljer: I Swift er kommandolinjeargumenter tilgjengelige gjennom 'CommandLine' -strukturen, som gir tilgang til alle argumentene som er gitt ved kjøring av programmet. Det finnes også tredjeparts biblioteker som kan gjøre lesing og prosessering av argumenter enklere.

Se også:
- [Swift Standard Library - CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [ArgumentParser - et tredjeparts bibliotek for kommandolinjeargumenter i Swift](https://github.com/apple/swift-argument-parser)