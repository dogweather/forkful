---
aliases:
- /fi/powershell/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:36.484239-07:00
description: "Standardivirheeseen (stderr) kirjoittaminen PowerShellissa tarkoittaa\
  \ virhesanomien tai diagnostiikkatietojen l\xE4hett\xE4mist\xE4 suoraan stderr-virtaan,\
  \ joka on\u2026"
lastmod: 2024-02-18 23:09:07.871675
model: gpt-4-0125-preview
summary: "Standardivirheeseen (stderr) kirjoittaminen PowerShellissa tarkoittaa virhesanomien\
  \ tai diagnostiikkatietojen l\xE4hett\xE4mist\xE4 suoraan stderr-virtaan, joka on\u2026"
title: Kirjoittaminen standardivirheeseen
---

{{< edit_this_page >}}

## Mikä & Miksi?

Standardivirheeseen (stderr) kirjoittaminen PowerShellissa tarkoittaa virhesanomien tai diagnostiikkatietojen lähettämistä suoraan stderr-virtaan, joka on erillinen standarditulostevirrasta (stdout). Tämä erottelu mahdollistaa tarkemman hallinnan skriptin tulosteen suhteen, mahdollistaen kehittäjille normaalien ja virhesanomien ohjaamisen eri kohteisiin, mikä on olennaista virheenkäsittelyssä ja lokitiedostojen kirjaamisessa.

## Kuinka:

PowerShell yksinkertaistaa stderriin kirjoittamisen prosessia käyttäen `Write-Error` cmdlet-komentoa tai ohjaamalla tulosteen `$host.ui.WriteErrorLine()` metodiin. Kuitenkin suoraa stderr-ohjausta varten saatat mieluummin käyttää .NET-menetelmiä tai PowerShellin itsensä tarjoamaa tiedostosuuntaimen uudelleenohjausta.

**Esimerkki 1:** `Write-Error` käyttö virhesanoman kirjoittamiseen stderriin.

```powershell
Write-Error "Tämä on virhesanoma."
```

Tuloste stderriin:
```
Write-Error: Tämä on virhesanoma.
```

**Esimerkki 2:** `$host.ui.WriteErrorLine()` käyttö suoraan stderr-kirjoitukseen.

```powershell
$host.ui.WriteErrorLine("Suora stderr-kirjoitus.")
```

Tuloste stderriin:
```
Suora stderr-kirjoitus.
```

**Esimerkki 3:** .NET-menetelmien käyttö kirjoittaessa stderriin.

```powershell
[Console]::Error.WriteLine("Käytetään .NET-menetelmää stderrille")
```

Tämän menetelmän tuloste:
```
Käytetään .NET-menetelmää stderrille
```

**Esimerkki 4:** Virhetulosteen uudelleenohjaus käyttämällä tiedostosuunnainta `2>`.

PowerShellissa tiedostosuuntimet voivat ohjata erilaisia virtoja. Stderrille tiedostosuunnain on `2`. Tässä on esimerkki, jossa stderr uudelleenohjataan tiedostoon nimeltä `error.log` suorittaessa komentoa, joka generoi virheen.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

Tämä esimerkki ei tuota konsolitulostetta, mutta generoi nykyiseen hakemistoon tiedoston `error.log`, joka sisältää virhesanoman yrittäessäsi käyttää olematonta tiedostoa.

Yhteenvetona, PowerShell tarjoaa useita menetelmiä virhetulosteen tehokkaaseen kirjaamiseen ja hallintaan, mahdollistaen kehittyneet virheenkäsittely- ja lokitiedostojen kirjausstrategiat skripteissä ja sovelluksissa.
