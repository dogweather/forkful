---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt innebærer å begynne på en helt ny kodebase, ofte for å oppnå en bestemt funksjon eller løsning. Programmerere gjør dette for å implementere ideer, løse problemer eller skape programvare som forbedrer eksisterende løsninger.

## Hvordan:

Opprett et nytt prosjekt i C# ved hjelp av `.NET Core CLI` (Command-Line Interface). Det er raskt og enkelt. Følg instruksjonene nedenfor:

```C#
// Åpne et kommandovindu og naviger til mappen hvor du vil opprette det nye prosjektet.
// Skriv kommandoen nedenfor for å opprette et nytt konsollprosjekt:

dotnet new console -n MittProsjekt

// Denne kommandoen vil opprette en ny mappe med navnet "MittProsjekt" med en enkel "Hello World" konsollapplikasjon.
// Du kan kjøre dette programmet ved å bruke følgende kommandoer:

cd MittProsjekt
dotnet run

// Output vil være:

Hello World!
```

## Dypdykk:

Historien om C# er ganske fascinerende, da språket er en del av .NET-plattformen som ble lansert av Microsoft i 2002. Alternativene til å starte et nytt prosjekt i C# inkluderer bruk av andre programmeringsspråk som Java, Python, eller JavaScript, avhengig av prosjektkravene.

En viktig detalj med implementeringen av et nytt prosjekt i C# er struktur og organisering av kode. C# følger et objektorientert programmeringsparadigme, noe som betyr at dataene er strukturert som objekter og klasser, noe som kan bidra til å opprettholde ren og effektiv kode.

## Se Også:

- Microsoft Docs (.NET Core CLI): [https://docs.microsoft.com/dotnet/core/tools/](https://docs.microsoft.com/dotnet/core/tools/)
- Introduksjon til C# - lærebok: [https://docs.microsoft.com/dotnet/csharp/tour-of-csharp/](https://docs.microsoft.com/dotnet/csharp/tour-of-csharp/)
- Grunnleggende objektorientert programmering i C#: [https://www.w3schools.com/cs/cs_oop.php](https://www.w3schools.com/cs/cs_oop.php)