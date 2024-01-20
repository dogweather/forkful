---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Behandle tekstfiler med PowerShell
## Hva og hvorfor?
Å lese en tekstfil innebærer i hovedsak å hente og tolke data lagret i en fil i tekstformat. Programmere gjør dette fordi det ofte er nødvendig å analysere eller manipulere disse dataene som en del av mange oppgaver og applikasjoner.

## Hvordan gjør man det?
Her er noen enkle eksempler på hvordan du leser en tekstfil ved hjelp av PowerShell. Vi vil bruke `Get-Content` cmdlet for dette formålet.

```PowerShell
# Les hele innholdet i en tekstfil
Get-Content -Path 'C:\sti\fil.txt'
```

```PowerShell
# Les de første fem linjene av en tekstfil
Get-Content -Path 'C:\sti\fil.txt' -First 5
```

```PowerShell
# Les de siste fem linjene av en tekstfil
Get-Content -Path 'C:\sti\fil.txt' -Last 5
```

## Dyp Dykk
Å lese filer er en fundamental funksjon i programmering. Det har alltid vært forskjellige måter å gjøre dette på tvers av forskjellige programmeringsspråk og teknologier. På PowerShell bruker vi `Get-Content` på grunn av dens kraftige funksjonalitet og fleksibilitet.

Alternativt kan du også utføre leseoperasjoner på filer ved hjelp av ".NET Framework methods", men for de fleste behov er det enklere og mer solid å bruke innebygde PowerShell cmdlets som `Get-Content`.

Når det gjelder implementeringsdetaljer, bruker `Get-Content` faktisk .NET-metoder under hetten for å utføre I/O-operasjoner på filen. Dette er et eksempel på hvordan PowerShell bygger på toppen av .NET-rammeverket for å forenkle oppgaver for utviklere.

## Se også
Her er noen lenker for å lese mer om å lese tekstfiler med PowerShell:
- [Offisiell PowerShell dokumentasjon for `Get-Content`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- [.NET dokumentasjon for File I/O-operasjoner](https://docs.microsoft.com/en-us/dotnet/standard/io/)