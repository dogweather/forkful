---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonojen pituuden löytäminen tallentaa merkkien lukumäärän merkkijonossa. Ohjelmoijat tarvitsevat tätä esimerkiksi silloin, kun heidän on varmistettava, että merkkijono sopii tiettyyn tilaan tai täyttää tiedonsiirron vaatimukset.

## Näin teet:
Yksinkertainen tapa selvittää merkkijonon pituus PowerShellillä on käyttää Length-ominaisuutta. Kokeilemme tässä esimerkissä:

```PowerShell
$merkkijono = "Hei maailma"
$merkkijono.Length
```

Tulostus on `11`, mikä tarkoittaa, että merkkijono sisältää 11 merkkiä (sisältäen välilyönnin).

## Syvemmälle
PowerShellin `Length`-ominaisuus liittyy .NET Frameworkiin, joka on ollut käytössä vuodesta 2002. Vaihtoehtoisia tapoja merkkijonon pituuden selvittämiseksi ovat esimerkiksi `Measure-Object` tai `StringInfo` .NET -luokan `LengthInTextElements` -metodi. Jälkimmäinen ottaa huomioon moninapaiset merkkijonot.

## Katso lisäksi
1. [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
2. [.NET Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
3. [StringInfo.LengthInTextElements Method](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo.lengthintextelements)

Huomaa, että PowerShellin dokumentaatio ja .NET -dokumentaatio ovat englanninkielisiä linkkejä, mutta voit käyttää sivuston sisäänrakennettua kääntäjää nähdäksesi sivut suomeksi. Jatka opiskelua ja koodaamista!