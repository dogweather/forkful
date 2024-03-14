---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:14.861509-07:00
description: "Een datum in de toekomst of het verleden berekenen betekent uitvogelen\
  \ welke datum het zal zijn na of voor een bepaalde tijdsperiode. Programmeurs doen\u2026"
lastmod: '2024-03-13T22:44:51.043633-06:00'
model: gpt-4-0125-preview
summary: "Een datum in de toekomst of het verleden berekenen betekent uitvogelen welke\
  \ datum het zal zijn na of voor een bepaalde tijdsperiode. Programmeurs doen\u2026"
title: Een datum in de toekomst of het verleden berekenen
---

{{< edit_this_page >}}

## Wat & Waarom?
Een datum in de toekomst of het verleden berekenen betekent uitvogelen welke datum het zal zijn na of voor een bepaalde tijdsperiode. Programmeurs doen dit om herinneringen te automatiseren, taken in te plannen of vervaldatums te beheren.

## Hoe te:

### Dagen toevoegen aan de huidige datum:
```PowerShell
# Voeg 10 dagen toe aan de datum van vandaag
$newDate = (Get-Date).AddDays(10)
Write-Output $newDate
```

Voorbeelduitvoer:
```
Donderdag 13 april 2023
```

### Dagen aftrekken van de huidige datum:
```PowerShell
# Trek 15 dagen af van vandaag
$pastDate = (Get-Date).AddDays(-15)
Write-Output $pastDate
```

Voorbeelduitvoer:
```
Woensdag 20 maart 2023
```

### Het verschil tussen twee datums berekenen:
```PowerShell
# Verschil tussen twee datums
$date1 = Get-Date '2023-04-01'
$date2 = Get-Date '2023-04-15'
$diff = $date2 - $date1
Write-Output $diff.Days
```

Voorbeelduitvoer:
```
14
```

## Diepere duik
Er was eens een tijd waarin programmeurs handmatig datums moesten berekenen met behulp van complexe algoritmen. Nu bieden talen zoals PowerShell ingebouwde functies zoals `AddDays`, `AddMonths`, wat het bijna triviaal maakt.

### Alternatieven:
Hoewel `AddDays` handig is, zijn er ook functies zoals `AddHours`, `AddMinutes`, enz., voor meer granulaire controle. Plus, je zou `[datetime]::Today.AddDays(10)` kunnen gebruiken als je een statische benadering prefereert.

### Implementatiedetails:
PowerShell's `DateTime`-object heeft deze methoden ingebouwd, dus je hoeft het wiel niet opnieuw uit te vinden. Onder de motorkap handelt het alle soorten complexiteiten af zoals schrikkeljaren en aanpassingen voor zomertijd.

## Zie ook
- PowerShell's officiële documentatie over `DateTime`-methoden: [Microsoft Docs - DateTime Methoden](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime?view=net-6.0)
- Meer over PowerShell datumarithmetiek: [PowerShell Datumarithmetiek](https://ss64.com/ps/syntax-dateformats.html)
- Om de geschiedenis en complexiteiten van kalendersystemen relevant voor datumcomputaties te verkennen: [The Calendar FAQ](http://www.tondering.dk/claus/cal/calendar29.html)
