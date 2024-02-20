---
date: 2024-01-20 17:31:47.519592-07:00
description: "\xC5 beregne en dato i fremtiden eller fortiden handler om \xE5 finne\
  \ datoen f\xF8r eller etter et gitt tidsintervall. Programmerere gj\xF8r dette for\
  \ \xE5 h\xE5ndtere\u2026"
lastmod: 2024-02-19 22:05:00.299808
model: gpt-4-1106-preview
summary: "\xC5 beregne en dato i fremtiden eller fortiden handler om \xE5 finne datoen\
  \ f\xF8r eller etter et gitt tidsintervall. Programmerere gj\xF8r dette for \xE5\
  \ h\xE5ndtere\u2026"
title: Beregning av en dato i fremtiden eller fortiden
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden handler om å finne datoen før eller etter et gitt tidsintervall. Programmerere gjør dette for å håndtere oppgaver som frister, planlegging, eller tidsbaserte funksjoner.

## Slik gjør du:
PowerShell gjør det lett å jobbe med datoer. Her ser du hvordan du legger til og trekker fra dager på dagens dato:

```PowerShell
# Legger til 10 dager
$tiDagerFrem = (Get-Date).AddDays(10)
Write-Output $tiDagerFrem

# Trekker fra 10 dager
$tiDagerTilbake = (Get-Date).AddDays(-10)
Write-Output $tiDagerTilbake
```

Prøve utfall:
```
fredag 10. mars 2023 14:00:00
tirsdag 18. februar 2023 14:00:00
```

## Dypdykk
I historisk kontekst har ulike kulturer og systemer målt tid på ulike måter. PowerShell anvender den gregorianske kalenderen, standarden for det meste av verden i dag. Alternativer for å beregne datoer inkluderer bruk av .NET-klasser i PowerShell, som `DateTime` og `TimeSpan`, eller tredjeparts verktøy og biblioteker. Når vi implementerer datoberegninger i PowerShell, støtter den direkte metodetilgang og objekter fra .NET-rammeverket, som lar deg manipulere datoer med stort presisjon og fleksibilitet.

## Se Også
For mer informasjon om håndtering av datoer og tider i PowerShell og .NET:

- [.NET API Documentation for DateTime Struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
