---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Väliaikaistiedosto on ohjelmoinnissa luotu tiedosto, joka tallentaa keskeneräisen työn tai palautuu, kun tehtävän suorittaminen on valmis. Tämä on hyödyllistä erityisesti läpimeneviä tietoja tai väliaikaisia tiedostoja varten, jotka saattavat syntyä suurten tiedostojen käsittelyn aikana.

## Kuinka tehdä:
Luodaksesi väliaikaisen tiedoston PowerShellissä, käytä ```New-TemporaryFile``` komentoa. Se luo väliaikaisen tiedoston sisäisessä temp-kansiossa. Tässä on miten se tehdään:

```PowerShell
$tempFile = New-TemporaryFile
$tempFile.Fullname
```

Ohjelma tulostaa väliaikaisen tiedoston sijainnin, joka näyttää suurin piirtein tältä:

```PowerShell
C:\Users\your-username\AppData\Local\Temp\tmp789.tmp
```

## Sukellus syvyyksiin
PowerShellin ```New-TemporaryFile``` komento julkaistiin Powershell 5.0 -versiossa vuonna 2016. Ennen sitä, kehittäjien täytyi käyttää .NET Framework -metodeja, kuten ```[System.IO.Path]::GetTempFileName()```.

Voit edelleen käyttää .NET Framework -menetelmiä niissä tilanteissa, joissa tarvitset laajennettuja ominaisuuksia, kuten määritetyn kansion, nimeä tai tiedoston laajennusta. 

Enimmäkseen, temp-tiedostot syntyy `C:\Users\your-username\AppData\Local\Temp\` polussa. Kuitenkin, voit muuttaa temp-tiedoston polkua ympäristömuuttujan `$env:TEMP` avulla.

## Katso myös
[PowerShellinen dokumentaation New-TemporaryFile](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7.1)

[.NET Frameworkin dokumentaatio GetTempFileName :sta](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)