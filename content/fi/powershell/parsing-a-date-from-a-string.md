---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "PowerShell: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Päivämäärän parsiminen merkkijonosta on prosessi, jossa muutetaan merkkijono sisäiseen päivämäärämuotoon. Tämä on tärkeää ohjelmoijille, jotta he voivat käsitellä ja vertailla päivämääriä tietokannoissa ja tiedostomuodoissa.

# Miten:

```PowerShell
$date = [datetime]::ParseExact("2020-12-25", "yyyy-MM-dd", $null)
Write-Output $date.ToShortDateString()
```

```PowerShell
$date = [datetime]::ParseExact("12/25/2020", "M/dd/yyyy", $null)
Write-Output $date.ToShortDateString()
```

```
25.12.2020
25.12.2020
```

# Syvällinen sukellus:

Päivämäärän parsimisessa on tärkeää määrittää oikea päivämäärämuoto, jotta parseri pystyy tulkitsemaan merkkijonon oikein. Tämä voi olla haastavaa etenkin, jos päivämäärät ovat kansainvälisessä muodossa – kuten esimerkiksi ensimmäinen esimerkki yllä, jossa kuukausi ja päivä on vaihdettu alkuruotsalaiseen tyyliin. On myös muita tapoja parsia päivämääriä PowerShellin avulla, kuten käyttämällä "Get-Date -Date" komentoa.

# Katso myös:

- [Microsoftin virallinen dokumentaatio päivämäärien parsimisesta PowerShellillä](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=netcore-3.1#System_DateTime_ParseExact_System_String_System_String_System_IFormatProvider_)
- [Päivämäärän parsiminen PowerShellin avulla YouTube-videossa (englanniksi)](https://www.youtube.com/watch?v=USXhLq9EEW8)