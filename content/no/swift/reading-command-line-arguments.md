---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese kommandolinjeargumenter handler om å hente brukerinput direkte fra systemets terminal. Dette gir oss en fleksibel og effektiv metode for å kontrollere hvordan programmet vårt skal kjøre, ved å tilpasse input basert på brukerens behov.

## Hvordan:

Å komme i gang med å lese kommandolinjeargumenter i Swift er enkelt. Du har tilgang til disse argumentene gjennom `CommandLine`-klassen.

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

Hvis du kjører programmet ditt med `swift myProgram.swift arg1 arg2`, vil output se slik ut:

```Swift
["myProgram.swift", "arg1", "arg2"]
```

## Deep Dive

Kommandolinjeargumenter har vært rundt siden tidlige dager av programmering, og er fortsatt sterkt brukt i dag. De gir en robust måte for brukere å interagere med programmet på. Mens det er andre alternativer, som å lage et grafisk brukergrensesnitt (GUI), gir kommandolinjeargumenter en enkel og direkte metode for input. 

Implementeringdetaljer i Swift inkluderer det faktum at `CommandLine.arguments` returnerer en liste av strenger. Den første strengen vil alltid være programnavnet, og resten vil være argumentene gitt av brukeren.

## Se Også

Sjekk ut disse nyttige linkene for mer informasjon og eksempler:

- Apple's Swift-programmeringsguide: [https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/)