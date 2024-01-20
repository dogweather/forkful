---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å starte et nytt prosjekt er det første skrittet mot å bygge et program eller en applikasjon fra bunnen av. Programmerere gjør dette for å oversette ideer til funksjonelle og nyttige verktøy. 

## Hvordan:

Her kommer noen enkle PowerShell-kommandoer som brukes til å opprette en ny mappe og en ny fil:

```PowerShell
# Lage en ny mappe
New-Item -Path 'C:\MinMappe' -ItemType Directory

# Lage en ny fil
New-Item -Path 'C:\MinMappe\MinFil.txt' -ItemType File
```

Når du kjører disse to kommandoene, vil du se noe sånt som dette:

```PowerShell
Directory: C:\

Mode                LastWriteTime         Length Name                              
----                -------------         ------ ----                              
d-----        4/14/2022  10:00 AM                MinMappe

Directory: C:\MinMappe

Mode                LastWriteTime         Length Name                              
----                -------------         ------ ----                              
-a----        4/14/2022  10:05 AM              0 MinFil.txt
```

## Dypdykk

Historisk sett ble prosjektstrukturering håndtert manuelt, noe som var tidskrevende og økte sannsynligheten for feil. PowerShell automatiserer denne prosessen, og tillater effektiv organisering av prosjekter.

Hvis du liker å utforske alternativer, er andre kommandolinje-skall som Bash og Zsh verdt å undersøke. Disse tilbyr lignende funksjonalitet med litt forskjellig syntax.

Når det gjelder implementeringsdetaljer, bruker `New-Item`-kommandoen `-ItemType`-parameteren for å bestemme hva som skal opprettes. Mulige verdier er "Directory", "File" og andre systemobjekter.

## Se Også

For mer informasjon om PowerShell, sjekk ut disse kildene:

- PowerShell dokumentasjon: [link](https://docs.microsoft.com/en-us/powershell/)
- Opprettelse av nye elementer: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/new-item?view=powershell-7.1)
- Om PowerShell prosjektstrukturering: [link](https://devblogs.microsoft.com/scripting/structuring-powershell-scripts/)