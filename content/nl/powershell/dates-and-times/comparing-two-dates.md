---
aliases:
- /nl/powershell/comparing-two-dates/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:41.358845-07:00
description: "Het vergelijken van twee datums in PowerShell betekent uitzoeken of\
  \ de ene eerder, hetzelfde, of later is dan de andere. Programmeurs doen dit vaak\
  \ om\u2026"
lastmod: 2024-02-18 23:09:02.105503
model: gpt-4-0125-preview
summary: "Het vergelijken van twee datums in PowerShell betekent uitzoeken of de ene\
  \ eerder, hetzelfde, of later is dan de andere. Programmeurs doen dit vaak om\u2026"
title: Twee datums vergelijken
---

{{< edit_this_page >}}

## Wat & Waarom?

Het vergelijken van twee datums in PowerShell betekent uitzoeken of de ene eerder, hetzelfde, of later is dan de andere. Programmeurs doen dit vaak om evenementen te beheren, records te sorteren, taken te plannen of de ouderdom van gegevens te controleren.

## Hoe:

```PowerShell
# Laten we de datum van vandaag pakken
$today = Get-Date

# En hier is een willekeurige datum
$someOtherDate = Get-Date "2023-03-17"

# Zijn ze gelijk?
$today -eq $someOtherDate

# Is vandaag groter (later) dan de andere datum?
$today -gt $someOtherDate

# Hoe zit het met controleren of het eerder is?
$today -lt $someOtherDate

# Laten we de resultaten eens bekijken, zullen we?

False
True
False
```

## Diepere Duik

Ver terug in de steentijd van het computeren—niet echt, maar, je weet wel, de vroege dagen—waren datums een rommeltje. We zijn een lange weg gekomen met standaarden en PowerShell maakt het nog eenvoudiger.

Hier zijn de stukken die het overpeinzen waard zijn:
1. **Geschiedenis**: Computers hanteerden vroeger datums in verschillende formaten, wat tot mogelijke verwarring en Y2K-achtige bugs kon leiden. PowerShell vertrouwt op de `DateTime` structuur van .NET, en vermijdt zulk een chaos.
   
2. **Alternatieven**: Je zou ook `Compare-Object` kunnen gebruiken, of methoden van `[datetime]` objecten zoals `AddDays()` om berekeningen uit te voeren vóór de vergelijking. Denk aan `Measure-Command` om de impact op de prestaties te testen.
   
3. **Implementatiedetails**: PowerShell-datums zijn objecten met hun eigen eigenschappen en methoden. Het vergelijken van datums gebeurt met operatoren (`-eq`, `-lt`, `-gt`), en dankzij operator overloading weet PowerShell dat je met datums bezig bent, niet slechts met strings of getallen.

Op het niveau van de assembly, vertaalt datumvergelijking naar ticks (100-nanoseconde-intervallen sinds 1/1/0001). Dus in wezen vergelijk je grote gehele getallen, wat efficiënt is.

## Zie Ook

- [DateTime Structuur (Microsoft Docs)](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime?view=net-6.0)
- [Werken met Datums en Tijden in PowerShell (SS64.com)](https://ss64.com/ps/syntax-dateformats.html)
