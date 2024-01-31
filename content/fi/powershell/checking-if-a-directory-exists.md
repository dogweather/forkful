---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:58:03.283462-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Tarkistetaan, onko hakemisto olemassa, jotta vältämme virheitä, kun yritämme käsitellä tiedostoja ja hakemistoja, jotka eivät ole olemassa. Se on järkevää, koska se säästää aikaa ja päänvaivaa.

## How to:
PowerShellin `Test-Path` komento kertoo, onko hakemisto olemassa.

```PowerShell
# Tarkistetaan onko hakemisto olemassa
$directoryPath = "C:\Example\MyFolder"

if (Test-Path $directoryPath) {
    Write-Host "Hakemisto löytyy."
} else {
    Write-Host "Hakemistoa ei ole olemassa."
}
```

Sample output, jos hakemisto on olemassa:

```
Hakemisto löytyy.
```

Jos hakemistoa ei ole, saat seuraavan:

```
Hakemistoa ei ole olemassa.
```

## Deep Dive
`Test-Path` on ollut osa PowerShellia sen alkupäivistä lähtien. Se toimii hyvin, koska on nopeaa ja helppoa tarkistaa, onko polku validi ja olemassa. Lisäksi `-PathType` parametrin avulla voimme spesifioida, etsimmekö tiedostoa vai hakemistoa.

Alternatiiveja, kuten [System.IO.Directory]::Exists($path) .NET-metodin käyttö, on olemassa, mutta ne ovat melko byrokraattisia PowerShell-skriptien kontekstissa.  

PowerShellin versiopäivitykset säilyttävät yleensä `Test-Path` komennon suorituskyvyn ja käyttäytymisen, joten ajan myötä ei ole tarvinnut huolehtia yhteensopivuusongelmista.

## See Also
PowerShellin dokumentaatio `Test-Path`-komennosta: [Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)

PowerShellin skriptausoppaat: [PowerShell Scripting](https://docs.microsoft.com/en-us/powershell/scripting/overview)

.Net API -dokumentaatio [Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists) metodista.
