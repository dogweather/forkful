---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:45.721355-07:00
description: 'Kuinka: #.'
lastmod: '2024-03-13T22:44:56.804168-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

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
