---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tarkistamme onko hakemisto olemassa, koska tiedostojen tai folderien käsittelyssä on tärkeää varmistaa, ettei törmätä olemattomiin hakemistoihin. Tämä estää virheilmoituksia ja ohjelman kaatumisia.

## Näin se tehdään:
Seuraava esimerkki näyttää kuinka voit tarkistaa, onko tietty hakemisto olemassa PowerShellilla:
```PowerShell
if (Test-Path -Path C:\esimerkki) {
    Write-Output "Hakemisto löytyy"
} else {
    Write-Output "Hakemisto ei ole olemassa"
}
```
Tämä koodi palauttaa joko "Hakemisto löytyy" tai "Hakemisto ei ole olemassa", riippuen siitä, löytyykö hakemisto "C:\esimerkki" vai ei.

## Syväluotaus
Tarkastellessamme, onko hakemisto olemassa, sen 'Test-Path' komento on ollut osa Powershellia sen ensimmäisestä versiosta lähtien. Test-Path on yksinkertainen ja suoraviivainen tapa tarkistaa, onko hakemisto tai tiedosto olemassa, mutta myös muita vaihtoehtoja, kuten 'Get-ChildItem' tai '.NET'-kirjastoja, voidaan käyttää samaan tarkoitukseen. Huomaathan, että 'Test-Path' komento palauttaa aina boolean arvon (tosi tai epätosi) riippumatta siitä, onko kyseessä tiedosto vai hakemisto.

## Lisätietoa
- Test-Path komennon tarkemmat tiedot: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1
- Hakemistot ja tiedostot PowerShellissa: https://docs.microsoft.com/en-us/powershell/scripting/samples/working-with-files-and-folders?view=powershell-7.1
- .NET-kirjaston FileSystemInfo-luokka: https://docs.microsoft.com/en-us/dotnet/api/system.io.filesysteminfo?view=net-5.0