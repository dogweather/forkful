---
title:                "Hente gjeldende dato"
html_title:           "PowerShell: Hente gjeldende dato"
simple_title:         "Hente gjeldende dato"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Få nåværende dato er rett og slett å få informasjon om dagens dato og klokkeslett. Dette er nyttig for programmerere når de jobber med tid- og datoavhengige funksjoner.

# Hvordan:
Det er flere forskjellige måter å få tak i nåværende dato på ved hjelp av PowerShell. Her er to enkle eksempler:

```PowerShell
# Eksempel 1: Bruk Get-Date-kommandoen for å få nåværende dato og klokkeslett.
Get-Date
```
Dette vil gi følgende output:
```
mandag 16.april 16:30:00
```

```PowerShell
# Eksempel 2: Bruk [datetime]::Now metoden for å få nåværende dato og klokkeslett.
[datetime]::Now
```
Dette vil gi følgende output:
```
mandag 16.april 16:30:00
```

# Dypdykk:
## Historisk kontekst:
Før PowerShell ble utviklet, var det vanlig å bruke kommandolinjeverktøyet "date" for å få tak i nåværende dato. Med PowerShell ble det enklere og mer fleksibelt å jobbe med datoer og tider.

## Alternativer:
I tillegg til Get-Date og [datetime]::Now, kan man også bruke [datetime]::UtcNow for å få nåværende universell tid. Det finnes også flere avanserte alternativer som kan brukes avhengig av behov.

## Implementeringsdetaljer:
Get-Date-kommandoen er innebygd i PowerShell og krever ingen ekstra moduler eller installasjoner. Den bruker systemets nåværende tid og dato for å gi output. [datetime]::Now metoden er en del av .NET Framework og kan brukes i alle .NET språk.

# Se også:
- Microsoft Docs: [Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
- Microsoft Docs: [DateTime.Now](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- PowerShell Prompt: [Date and Time Formatting in PowerShell](https://www.powershellprompt.com/post/date-and-time-formatting-in-powershell)