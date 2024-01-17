---
title:                "Tiedostojen lukeminen"
html_title:           "PowerShell: Tiedostojen lukeminen"
simple_title:         "Tiedostojen lukeminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstin lukeminen tarkoittaa yksinkertaisesti tekstitiedoston sisällön lukemista ja käyttämistä koodissa. Ohjelmoijat voivat käyttää tekstitiedostoja esimerkiksi konfiguraatiotietojen tallentamiseen tai tietojen lataamiseen ohjelmaan. 

## Kuinka:
```PowerShell
# Oletetaan, että meillä on tekstitiedosto nimeltä "esimerkki.txt"
# Luetaan tiedoston sisältö
$sisältö = Get-Content -Path ".\esimerkki.txt"

# Tulostetaan sisältö näytölle
Write-Output $sisältö

# Voimme myös käyttää tekstitiedostoa koodissa
# Esimerkiksi tallentamalla sisällön muuttujaan ja käyttämällä sitä muissa toiminnoissa
# $sisältö = Get-Content -Path ".\esimerkki.txt"
# Do-Something $sisältö
```

## Syvemmälle:
Tekstin lukeminen on ollut osa ohjelmointia jo pitkään ja siitä on tullut välttämätön osa monia ohjelmointikieliä. PowerShellissa tekstitiedostoja voidaan lukea useilla eri tavoilla, kuten Get-Content, ReadAllText, tai FileStream-olioon perustuen. Vaihtoehtoja on siis useita, mutta Get-Content on yleensä suositeltu tapa lukemiseen, koska se tekee siitä yksinkertaista ja helppoa. 

## Katso myös:
- [Get-Content](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Get-Content)
- [ReadAllText](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Content)
- [FileStream-olio](https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream?view=netframework-4.8)