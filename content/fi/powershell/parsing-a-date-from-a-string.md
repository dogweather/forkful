---
title:                "Päivämäärän jäsennys merkkijonosta"
date:                  2024-02-03T19:15:26.213083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa tekstissä kirjoitettujen päivämäärien tunnistamista ja muuntamista päivämäärätyypiksi, jonka PowerShell ymmärtää ja jolla se voi työskennellä. Ohjelmoijat tekevät tämän manipuloidakseen, muotoillakseen, vertailakseen tai lasketakseen päivämääriä, mikä on yleinen tehtävä skripteissä, jotka käsittelevät lokitiedostoja, käyttäjän syötteitä tai datan prosessointia.

## Kuinka:
PowerShell tekee päivämäärän jäsentämisen merkkijonoista suoraviivaista `Get-Date` cmdlet:llä ja `[datetime]` tyypin kiihdyttimellä, jotka toimivat hyvin standardimuotoisille päivämäärille. Monimutkaisempia tai ei-standardimuotoisia päivämäärämerkkijonoja varten voidaan käyttää `[datetime]::ParseExact` metodia määrittelemään tarkka muoto.

### Käyttäen `Get-Date` ja `[datetime]`:
```powershell
# Yksinkertainen muunnos käyttäen Get-Datea
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Esimerkkituloste:**
```
Lauantai, huhtikuu 1, 2023 00:00:00
```

```powershell
# Käyttäen tyypin kiihdytintä [datetime]
$stringDate = "huhtikuu 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Esimerkkituloste:**
```
Lauantai, huhtikuu 1, 2023 00:00:00
```

### Käyttäen `[datetime]::ParseExact` ei-standardimuotoille:
Muodoille, joita ei automaattisesti tunnisteta, voit määritellä tarkan muodon varmistaaksesi oikean jäsentämisen.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Esimerkkituloste:**
```
Lauantai, huhtikuu 1, 2023 14:00:00
```

### Hyödyntäen kolmannen osapuolen kirjastoja
Vaikka PowerShell itsessään on melko tehokas päivämäärien jäsentämiseen, erittäin monimutkaisissa skenaarioissa tai lisätoiminnoissa saatat tutkia .NET-kirjastoja, kuten NodaTime, vaikka monille tyypillisille käyttötapauksille PowerShellin omat kyvyt riittävät.

```powershell
# Käyttäen NodaTimea vain havainnollistuksena, huomaa että sinun tulee lisätä kirjasto projektiisi
# Install-Package NodaTime -Version 3.0.5
# Käyttäen NodaTimea päivämäärän jäsentämiseen
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Huomautus esimerkistä:** Yllä oleva koodi on käsitteellinen havainnollistus. Käytännössä varmista, että NodaTime on oikein lisätty projektiisi, jotta tyypit ja metodit ovat käytettävissä.
