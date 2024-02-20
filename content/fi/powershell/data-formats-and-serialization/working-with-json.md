---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:45.721355-07:00
description: "PowerShellin integraatio JSONiin (JavaScript Object Notation) liittyy\
  \ JSON-tietojen j\xE4sent\xE4miseen (lukeminen) ja tuottamiseen (kirjoittaminen),\
  \ mik\xE4 on\u2026"
lastmod: 2024-02-19 22:05:15.699701
model: gpt-4-0125-preview
summary: "PowerShellin integraatio JSONiin (JavaScript Object Notation) liittyy JSON-tietojen\
  \ j\xE4sent\xE4miseen (lukeminen) ja tuottamiseen (kirjoittaminen), mik\xE4 on\u2026"
title: "Ty\xF6skentely JSON:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

PowerShellin integraatio JSONiin (JavaScript Object Notation) liittyy JSON-tietojen jäsentämiseen (lukeminen) ja tuottamiseen (kirjoittaminen), mikä on yleinen muoto tietojen vaihtoon verkossa. Ohjelmoijat työskentelevät JSONin kanssa ollakseen vuorovaikutuksessa verkon API:en kanssa, konfiguraatiotiedostojen kanssa tai helpottaakseen tietojen vaihtoa eri kielten ja alustojen välillä sen kevytrakenteisen ja kieli-riippumattoman luonteen vuoksi.

## Kuinka:

### JSONin jäsentäminen

JSONin lukemiseksi tai jäsentämiseksi PowerShellissa voit käyttää `ConvertFrom-Json` cmdletiä. Annetulle JSON-merkkijonolle tämä cmdlet muuntaa sen PowerShell-objektiksi.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Esimerkkituloste:

```
John Doe
```

Tämä esimerkki havainnollistaa, kuinka jäsentää yksinkertainen JSON-merkkijono ja päästä käsiksi tuloksen ominaisuuksiin.

### JSONin tuottaminen

JSONin tuottamiseksi PowerShell-objektista voit käyttää `ConvertTo-Json` cmdletiä. Tämä on kätevää valmisteltaessa tietoja lähetettäväksi verkkopalveluun tai tallennettavaksi konfiguraatiotiedostoon.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Esimerkkituloste:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

Tämä koodinpätkä luo PowerShell-objektin ja muuntaa sen sitten JSON-merkkijonoksi.
