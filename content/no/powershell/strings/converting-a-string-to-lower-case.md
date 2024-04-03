---
date: 2024-01-20 17:39:23.866101-07:00
description: "Hvordan gj\xF8re det: Bruk `ToLower()` metoden for \xE5 konvertere strenger."
lastmod: '2024-03-13T22:44:41.003673-06:00'
model: gpt-4-1106-preview
summary: "Bruk `ToLower()` metoden for \xE5 konvertere strenger."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## Hvordan gjøre det:
Bruk `ToLower()` metoden for å konvertere strenger:

```PowerShell
$streng = "Hei, Norge!"
$småBokstaver = $streng.ToLower()
Write-Output $småBokstaver
```

Forventet resultat:

```
hei, norge!
```

## Dypdykk:
Historisk sett, handler denne konverteringen om konsistens i data og var en del av tidlige datamaskiners kapasiteter for tekstbehandling. Alternativer til `ToLower()` inkluderer `ToUpper()` for å endre til store bokstaver eller bruk av regulære uttrykk for mer kontroll. Når du bruker `ToLower()`, påvirkes ikke tall eller tegnsetting, kun bokstaver fra A til Å.

PowerShell bruker kulturelt sensitiv `ToLower()` metode som standard, som tar hensyn til lokale konvensjoner basert på maskinens innstillinger eller kultur parameter. Hvis du trenger en kultur-uavhengig operasjon, bruk `ToLowerInvariant()`.

```PowerShell
$streng = "BRØD OG SKI!"
$kultureltSmåBokstaver = $streng.ToLower('nn-NO') # Nynorsk - Norge
$kulturUavhengigSmåBokstaver = $streng.ToLowerInvariant()
Write-Output $kultureltSmåBokstaver
Write-Output $kulturUavhengigSmåBokstaver
```

Vær oppmerksom på at kultur-spesifikke endringer kan forekomme, for eksempel med tyrkisk 'İ' blir det til 'i' når du bruker `ToLower()` med den tyrkiske kulturkoden, men til 'ı' med `ToLowerInvariant()`.

## Se Også:
- PowerShell dokumentasjon om strenger: https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-string-data-type?view=powershell-7.1
- Om Unicode og tekstbehandling: https://unicode.org
- Microsoft guide til kulturelle konvensjoner i .NET: https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/culture-and-cultureinfo
