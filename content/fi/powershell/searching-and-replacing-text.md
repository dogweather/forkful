---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "PowerShell: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hakemiston ja tekstin vaihtamisella tarkoitetaan tekstin etsimistä ja korvaamista toisella tekstillä tietokoneen ohjelmassa tai tiedostossa. Ohjelmoijat käyttävät tätä prosessia esimerkiksi virheiden korjaamiseen tai tekstin muokkaamiseen suuressa määrin.

## Kuinka:

```PowerShell
# Etsi ja korvaa teksti tiedostosta
(Get-Content -Path "polku\tekstitiedosto.txt") -replace "vanha", "uusi" | Set-Content -Path "polku\muokattu_tekstitiedosto.txt"

# Etsi ja korvaa teksti merkkijonosta
"Tervehdys, vanha maailma" -replace "vanha", "uusi"
```

### Tuloste:
```
Tervehdys, uusi maailma
```

## Syväsukellus:

Tekstin etsiminen ja korvaaminen on tärkeä osa ohjelmoijien työtä, sillä se auttaa heitä korjaamaan virheitä ja nopeuttamaan tekstin muokkausprosessia. Historiallisessa kontekstissa, tähän prosessiin on käytetty erilaisia työkaluja ja menetelmiä, kuten Regular Expression (regex). PowerShellin tapauksessa, käytämme -replace komentoa, joka on osa sen ydintoimintoja ja mahdollistaa tekstin etsimisen ja korvaamisen helposti.

## Katso myös:

- [PowerShell - Dokumentaatio](https://docs.microsoft.com/fi-fi/powershell/scripting/overview?view=powershell-7)
- [PowerShell - Advanced Functions](https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.core/about/about_functions_advanced?view=powershell-7)
- [Gratissoftwares.com - Tekstin vaihtaminen Notepad++ ohjelmalla](https://www.gratissoftwares.com/tekstin-vaihtaminen-notepad-ohjelmalla)