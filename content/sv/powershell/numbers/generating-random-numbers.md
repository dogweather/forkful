---
date: 2024-01-27 20:35:11.908317-07:00
description: "Att generera slumpm\xE4ssiga tal i PowerShell handlar om att skapa of\xF6\
  ruts\xE4gbara numeriska v\xE4rden inom ett angivet intervall. Programmerare anv\xE4\
  nder denna\u2026"
lastmod: '2024-03-11T00:14:11.503205-06:00'
model: gpt-4-0125-preview
summary: "Att generera slumpm\xE4ssiga tal i PowerShell handlar om att skapa of\xF6\
  ruts\xE4gbara numeriska v\xE4rden inom ett angivet intervall. Programmerare anv\xE4\
  nder denna\u2026"
title: Generera slumptal
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal i PowerShell handlar om att skapa oförutsägbara numeriska värden inom ett angivet intervall. Programmerare använder denna förmåga av en mängd olika anledningar, inklusive testning, simulering och säkerhetssyften, där oförutsägbarhet eller efterliknande av verklig slumpmässighet är avgörande.

## Hur man gör:
PowerShell erbjuder en enkel metod för att generera slumpmässiga tal med hjälp av cmdleten `Get-Random`. Denna cmdlet kan producera slumpmässiga tal inom ett standardintervall eller ett angivet intervall.

```PowerShell
# Generera ett slumpmässigt tal mellan 0 och Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

För att specificera ett intervall, använd parametrarna `-Minimum` och `-Maximum`:

```PowerShell
# Generera ett slumpmässigt tal mellan 1 och 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

För mer kontroll kan du instansiera ett objekt av klassen `System.Random`:

```PowerShell
# Använda System.Random för en sekvens av tal
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Om du behöver ett slumpmässigt urval från en array eller samling kan `Get-Random` direkt välja ett objekt:

```PowerShell
# Slumpmässigt urval från en array
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Fördjupning
Cmdleten `Get-Random` i PowerShell utnyttjar .NET-klassen `System.Random` bakom kulisserna för att generera pseudoslumpmässiga tal. Dessa är "pseudo" eftersom de använder algoritmer för att producera sekvenser av tal som bara verkar slumpmässiga. För de flesta applikationer är denna nivå av slumpmässighet tillräcklig. Dock, för användningsfall som kräver kryptografisk säkerhet, är `System.Random` inte lämplig på grund av sin förutsägbara natur.

PowerShell och .NET erbjuder `System.Security.Cryptography.RNGCryptoServiceProvider` för kryptografisk slumpmässighet, vilket är mer lämpligt för att generera krypteringsnycklar eller andra säkerhetskänsliga operationer:

```PowerShell
# Kryptografiskt säkra slumpmässiga tal
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Även om `Get-Random` och `System.Random` tillfredsställer ett brett spektrum av behov för slumpmässighet i skriptning och applikationslogik, är det avgörande att välja rätt verktyg för jobbet, särskilt i säkerhetscentrerade applikationer där förutsägbarhet kan utgöra en sårbarhet.
