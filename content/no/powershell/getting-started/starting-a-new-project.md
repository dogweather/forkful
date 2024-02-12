---
title:                "Å starte et nytt prosjekt"
aliases:
- /no/powershell/starting-a-new-project/
date:                  2024-01-20T18:04:05.072650-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å starte et nytt prosjekt"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å starte et nytt prosjekt betyr å opprette grunnlaget for kodearbeidet ditt. Programmerere gjør dette for å organisere kode, dele arbeidet i håndterbare deler og lettere kunne vedlikeholde det i fremtiden.

## Slik gjør du:
Koden nedenfor viser hvordan du kan sette opp et nytt prosjekt ved hjelp av PowerShell:

Opprett en ny mappe:
```PowerShell
New-Item -Path 'C:\mine_prosjekter\MittNyProsjekt' -ItemType Directory
```

Naviger til din nye mappe:
```PowerShell
Set-Location -Path 'C:\mine_prosjekter\MittNyProsjekt'
```

Initialiser et Git-repositorium (valgfritt):
```PowerShell
git init
```
Output:
```
Initialized empty Git repository in C:/mine_prosjekter/MittNyProsjekt/.git/
```
Opprett en ny PowerShell scriptfil:
```PowerShell
New-Item -Path '.\Start-Skript.ps1' -ItemType File
```

## Dypdykk:
Å starte et nytt prosjekt har variert litt gjennom årene. Tidligere måtte man ofte manuelt konfigurere mange aspekter, mens moderne verktøy som PowerShell automatiserer og forenkler prosessen. Det er alternativer til PowerShell, som Bash på Linux eller Zsh på MacOS, men PowerShell er innsvevet i Windows og tilbyr en rikdom av cmdlets designet for systemadministrasjon.

En viktig vurdering er prosjektstruktur og -oppsett. Dette kan inkludere å velge riktige mapper og oppsette versjonskontroll med Git, som ikke bare hjelper med historikken til koden, men gjør det også enklere for flere utviklere å samarbeide. I tillegg til manuelle metoder som vi har gjennomgått, er det også verktøy som PowerShell-modulen 'Plaster' som kan hjelpe deg med å automatisere opprettelse av prosjektstrukturer.

## Se Også:
- Microsofts egen dokumentasjon for PowerShell: https://docs.microsoft.com/en-us/powershell/
- En introduksjon til versjonskontroll med Git: https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control
- Om PowerShell-modulen 'Plaster': https://github.com/PowerShell/Plaster
