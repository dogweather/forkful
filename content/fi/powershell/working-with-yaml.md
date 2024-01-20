---
title:                "Työskentely yaml:n kanssa"
html_title:           "PowerShell: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

YAML on yksinkertainen tapa tallentaa tietoja tekstimuodossa, esimerkiksi asetustiedostoja tai tietokantatietoja. Ohjelmoijat käyttävät YAMLia, koska se on helppo lukea ja muokata, ja se mahdollistaa tietojen tallentamisen rakenteellisesti ilman monimutkaisia tietokantoja.

## Kuinka:

```PowerShell
# Tallentaminen YAML-tiedostoksi
$asetukset = @{
    Kieli = "suomi"
    Laitteet = "kannettava, puhelin, älykello"
}
$asetukset | ConvertTo-YAML | Out-File "asetukset.yml"

# Lukea YAML-tiedostosta
$asetukset = Get-Content "asetukset.yml" | ConvertFrom-YAML
# Tulostetaan asetukset
Write-Host "Nykyinen kieli:" $asetukset.Kieli
Write-Host "Käytettävät laitteet:" $asetukset.Laitteet
```

**Tuloste:**

```
Nykyinen kieli: suomi
Käytettävät laitteet: kannettava, puhelin, älykello
```

## Syvemmälle:

1. YAML kehitettiin vuonna 2001, mutta sen suosio ohjelmoijien keskuudessa on kasvanut merkittävästi viime vuosina.
2. Vaihtoehtoisia tapoja tallentaa tietoja tekstimuodossa ovat esimerkiksi JSON ja XML.
3. PowerShellin `ConvertTo-YAML` ja `ConvertFrom-YAML` komennot mahdollistavat YAML-tiedostojen luomisen ja lukemisen suoraan PowerShell-konsolissa ilman lisäohjelmia.

## Katso myös:

- [YAML.org](https://yaml.org/) - YAMLin kotisivu.
- [PowerShell-komentoikkunan dokumentaatio](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/?view=powershell-7.1#convertto-yaml) - Lisätietoja `ConvertTo-YAML` ja `ConvertFrom-YAML` komennoista.