---
date: 2024-01-20 17:56:41.481245-07:00
description: "Slik gj\xF8r du: I Swift er kommandolinjeargumentene tilgjengelige via\
  \ `CommandLine.arguments`-arrayet. Her er et enkelt eksempel."
lastmod: '2024-03-13T22:44:41.157408-06:00'
model: gpt-4-1106-preview
summary: I Swift er kommandolinjeargumentene tilgjengelige via `CommandLine.arguments`-arrayet.
title: Lese kommandolinjeargumenter
weight: 23
---

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
