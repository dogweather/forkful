---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt handler om å sette opp et rammeverk for programmeringsarbeidet ditt i henhold til din plan. Programmerere gjør dette for å ha en strømlinjeformet kodegenerering og effektiv feilsøking.

## Hvordan å:

Å lage et nytt Swift-prosjekt er relativt enkelt. Åpne Terminal og bruk `swift package init`-kommandoen for å opprette et nytt prosjekt.

```Swift
$ mkdir MittNyeProsjekt
$ cd MittNyeProsjekt
$ swift package init --type executable
```

For å kjøre dette prosjektet, er det bare å bruke `swift run`-kommandoen.

```Swift
$ swift run
```

Utfallet av ovennevnte kodeblokker vil være et kjørbar Swift-prosjekt med den angitte navn og strukturen.

## Dypdykk:

Å starte et nytt prosjekt har lenge vært en integrert del av generell programmeringspraksis, tilpasset behovene til forskjellige språk og rammer opp gjennom årene.

Som et alternativ til Swift Package Manager (SPM), kan du også bruke IDE-er som Xcode for å starte et nytt prosjekt. Xcode tilbyr et GUI-alternativ og flere praktiske funksjoner, men SPM gir mer kontroll og fleksibilitet i prosjektstyringen.

Når det gjelder implementeringsdetaljer, oppretter `swift package init --type executable` en grunnleggende prosjektmappstruktur for deg, inkludert et testrammeverk og en kildekodemappe med en grunnleggende "Hello, World!" script.

## Se Også:

- Swift Package Manager Dokumentasjon - https://swift.org/package-manager/
- Swift Xcode-prosjektoppretting - https://developer.apple.com/library/archive/documentation/ToolsLanguages/Conceptual/Xcode_Overview/Creating_a_New_Xcode_Project.html
- Apple Swift Programmeringsspråkghåndbok - https://docs.swift.org/swift-book/