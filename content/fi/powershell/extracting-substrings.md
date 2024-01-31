---
title:                "Merkkijonojen osien poimiminen"
date:                  2024-01-20T17:46:32.838555-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Substringien poiminta on prosessi, jolla valikoidaan ja otetaan tietty osa merkkijonosta. Ohjelmoijat tekevät tätä, koska usein tarvitaan vain pala dataa esimerkiksi analyysia tai formaattia varten.

## How to: (Kuinka tehdä:)
```PowerShell
# Otetaan esimerkiksi merkkijono
$esimerkkiMerkkijono = "Tämä on esimerkki merkkijono"

# Substring alkaa indeksistä 8 ja päättyy indeksiin 17
$substring = $esimerkkiMerkkijono.Substring(8, 10)
Write-Host $substring
```

Tulostus:
```
esimerkki
```

```PowerShell
# Voit myös käyttää -split operaattoria ja valita haluamasi osan
$osat = $esimerkkiMerkkijono -split ' '
Write-Host $osat[3]
```

Tulostus:
```
esimerkki
```

## Deep Dive (Syväsukellus)
Substringien poiminnalla on pitkä historia ohjelmoinnissa; se on perusoperaatio, jota on käytetty lähes kaikissa ohjelmointikielissä tiedon käsittelyyn. PowerShellissä `.Substring()` metodi ja `-split` operaattori ovat yleisimpiä tapoja tehdä tämä.  Niiden toteutus hyödyntää .NET -alustan luokkakirjastoja, jotka ovat tehokkaita ja optimoituja. Vaihtoehtoisia menetelmiä ovat esimerkiksi regex -ilmaisut tai string-metodit, kuten `.IndexOf()` yhdistettynä `.Remove()`- tai `.Replace()`-metodeihin.

## See Also (Katso Myös)
- PowerShellin string operaattorit: [about Split operator in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7.1)
