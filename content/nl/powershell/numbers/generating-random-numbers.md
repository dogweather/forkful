---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:51.826556-07:00
description: "Het genereren van willekeurige getallen in PowerShell gaat over het\
  \ cre\xEBren van onvoorspelbare numerieke waarden binnen een gespecificeerd bereik.\u2026"
lastmod: '2024-03-13T22:44:51.022875-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in PowerShell gaat over het cre\xEB\
  ren van onvoorspelbare numerieke waarden binnen een gespecificeerd bereik."
title: Willekeurige getallen genereren
weight: 12
---

## Wat & Waarom?
Het genereren van willekeurige getallen in PowerShell gaat over het creëren van onvoorspelbare numerieke waarden binnen een gespecificeerd bereik. Programmeurs gebruiken deze mogelijkheid om uiteenlopende redenen, waaronder testen, simulatie en beveiligingsdoeleinden, waar onvoorspelbaarheid of het nabootsen van real-world willekeur cruciaal is.

## Hoe te:
PowerShell biedt een eenvoudige aanpak om willekeurige getallen te genereren met behulp van de `Get-Random` cmdlet. Deze cmdlet kan willekeurige getallen produceren binnen een standaardbereik of een gespecificeerd bereik.

```PowerShell
# Genereer een willekeurig getal tussen 0 en Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Om een bereik op te geven, gebruik je de `-Minimum` en `-Maximum` parameters:

```PowerShell
# Genereer een willekeurig getal tussen 1 en 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Voor meer controle kun je een object van de `System.Random` klasse instantiëren:

```PowerShell
# Gebruik System.Random voor een reeks van getallen
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Als je een willekeurige selectie uit een array of collectie nodig hebt, kan `Get-Random` direct een item kiezen:

```PowerShell
# Willekeurige selectie uit een array
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Diepgaand
De `Get-Random` cmdlet in PowerShell maakt onder de motorkap gebruik van de .NET-klasse `System.Random` om pseudowillekeurige getallen te genereren. Deze zijn "pseudo" omdat ze algoritmen gebruiken om reeksen getallen te produceren die alleen willekeurig lijken. Voor de meeste toepassingen is dit niveau van willekeur voldoende. Echter, voor gebruiksscenario's die cryptografische beveiliging vereisen, is `System.Random` niet geschikt vanwege zijn voorspelbare aard.

PowerShell en .NET bieden `System.Security.Cryptography.RNGCryptoServiceProvider` voor cryptografische willekeur, wat meer geschikt is voor het genereren van encryptiesleutels of andere beveiligingsgevoelige operaties:

```PowerShell
# Cryptografisch veilige willekeurige getallen
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Hoewel `Get-Random` en `System.Random`een brede reeks behoeften voor willekeur in scripting en applicatielogica bevredigen, is het essentieel om het juiste gereedschap voor de klus te kiezen, vooral in beveiligingsgerichte toepassingen waar voorspelbaarheid een kwetsbaarheid kan vormen.
