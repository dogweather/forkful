---
title:                "Lese kommandolinjeargumenter"
aliases:
- /no/swift/reading-command-line-arguments.md
date:                  2024-01-20T17:56:41.481245-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av kommandolinjeargumenter lar programmer reagere på input de får når de startes fra terminalen. Programmerere bruker dette til å tilpasse kjøringen av programmet uten å hardkode verdier.

## Slik gjør du:
I Swift er kommandolinjeargumentene tilgjengelige via `CommandLine.arguments`-arrayet. Her er et enkelt eksempel:

```Swift
// main.swift

for argument in CommandLine.arguments {
    print(argument)
}
```

Kjør programmet med `swift main.swift arg1 arg2 arg3`:

```Swift
// Utdata
/main.swift
arg1
arg2
arg3
```

## Dypdykk
Historisk sett har kommandolinjeargumenter vært en integrert del av mange programmeringsspråk, og Swift er ikke noe unntak. Alternativer til `CommandLine.arguments` inkluderer å bruke biblioteker som `Swift Argument Parser` for mer avansert parsing. Når det gjelder implementasjonsdetaljer: `CommandLine` er egentlig en forenklet wrapper rundt argc (argument count) og argv (argument vector), som er standard C-mekanismer for argumenthåndtering.

## Se også
- [Swift Argument Parser GitHub](https://github.com/apple/swift-argument-parser)
- [Apple's CommandLine Documentation](https://developer.apple.com/documentation/swift/commandline)
