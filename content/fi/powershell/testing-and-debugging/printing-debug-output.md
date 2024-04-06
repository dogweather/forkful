---
date: 2024-01-20 17:53:20.875049-07:00
description: "How to: (Kuinka tehd\xE4:) PowerShellissa debug-viestej\xE4 voi tulostaa\
  \ `Write-Host`, `Write-Output`, `Write-Verbose`, `Write-Debug` ja `Write-Information`\u2026"
lastmod: '2024-04-05T21:53:58.359426-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) PowerShellissa debug-viestej\xE4 voi tulostaa `Write-Host`,\
  \ `Write-Output`, `Write-Verbose`, `Write-Debug` ja `Write-Information` komentojen\
  \ avulla."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to: (Kuinka tehdä:)
PowerShellissa debug-viestejä voi tulostaa `Write-Host`, `Write-Output`, `Write-Verbose`, `Write-Debug` ja `Write-Information` komentojen avulla. Tässä muutama esimerkki:

```PowerShell
# Yksinkertainen tulostus
Write-Host "Tämä on tavallinen viesti."

# Muuttujan arvon tulostus
$muuttuja = "arvokas tieto"
Write-Host "Muuttujan arvo on: $muuttuja"

# Debug-viestin tulostus
Write-Debug "Debug-viesti näkyy, jos $DebugPreference on 'Continue'."

# Kehittynyt käyttö: custom-objektin tiedot
$objekti = [PSCustomObject]@{
    Nimi = 'PowerShell'
    Versio = '7.1.3'
}
Write-Output $objekti
```

Huomaa, että `Write-Debug` ja `Write-Verbose` vaativat tietyt asetukset ollakseen näkyviä.

Tulosteena nähdään:
```
Tämä on tavallinen viesti.
Muuttujan arvo on: arvokas tieto
DEBUG: Debug-viesti näkyy, jos $DebugPreference on 'Continue'.
@{Nimi=PowerShell; Versio=7.1.3}
```

## Deep Dive (Syvä sukellus)
Historiallisesti debug-viestit ovat olleet kehittäjän työkaluja ohjelmiston toiminnan ymmärtämiseksi. PowerShellissä `Write-Debug` ja `Write-Verbose` ovat työkaluja syvempään analyysiin, mutta niiden viestit näkyvät vain, kun asetukset sallivat. Vaihtoehtoisia tapoja ovat esim. tiedostojen kirjoittaminen tai järjestelmän lokiviestit.

Kehittäjinä meidän on tärkeä ymmärtää, milloin ja mitä tulostusvälinettä käyttää. `Write-Host` muuttaa väriä ja muotoilua helpommin, kun taas `Write-Output` on suunniteltu välittämään objekteja eteenpäin komentoputkessa. `Write-Debug` ja `Write-Verbose` antavat yksityiskohtasia tietoja, mutta voivat häiritä tavallista käyttöä, eli niitä käytetään harvemmin. `Write-Information` voidaan säätää näyttämään viestit ilman, että sekatkaa tulostusta.

## See Also (Katso myös)
Lisätietolähteitä ja lisäohjeita:

- [about_Write-Host](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-host)
