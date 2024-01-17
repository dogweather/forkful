---
title:                "Å starte et nytt prosjekt"
html_title:           "C#: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Når du står overfor et nytt programmeringsprosjekt, er det viktig å starte på riktig måte. Det betyr å opprette et nytt prosjekt fra grunnen av. Dette innebærer å sette opp et miljø for utvikling som best passer dine behov, og skrive grunnleggende kode for å få prosjektet i gang.

Hvordan:
Det første trinnet i å starte et nytt prosjekt er å opprette en ny C#-løsning i Visual Studio. Deretter kan du legge til prosjekt- og filstrukturer, samt nødvendige biblioteker og pakker. Ta gjerne en titt på følgende kodeeksempler for å se hvordan dette kan gjøres:

```
C# // Opprettelse av en ny løsning
dotnet new sln -n MinLøsning

// Oppretting av prosjekt og filstruktur
dotnet new console -n MinApplikasjon
cd MinApplikasjon
dotnet add reference ../MinLøsning.sln

// Legge til nødvendige biblioteker og pakker
dotnet add package Navn-på-pakken
```

Du kan også bruke Visual Studio-oppsettveiviseren for å opprette et nytt prosjekt. Når grunnleggende strukturer er på plass, kan du begynne å skrive kode for å få prosjektet ditt til å gjøre det du ønsker det skal gjøre.

Dypdykk:
Å starte et nytt prosjekt er et viktig første skritt i enhver utviklingsoppgave. Det gir deg muligheten til å velge ditt foretrukne miljø og sett med verktøy som best støtter dine behov. Selv om vi brukte Visual Studio som et eksempel, er det viktig å nevne at det finnes andre alternative utviklingsmiljøer for C#, som Visual Studio Code og JetBrains Rider.

Se også:
- https://docs.microsoft.com/en-us/visualstudio/get-started/csharp/tutorial-console?view=vs-2019
- https://visualstudio.microsoft.com/
- https://code.visualstudio.com/
- https://www.jetbrains.com/rider/