---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Tekstin muuttaminen isolla alkukirjaimella kirjoitettuun muotoon tarkoittaa joko koko merkkijonon tai jokaisen sanan ensimmäisen kirjaimen muuttamista isoksi. Ohjelmoijat käyttävät tätä muotoilua usein käyttöliittymissä, asiakirjojen otsikoissa tai aina kun tiettyä tekstiä halutaan korostaa.

## How to: (Kuinka tehdä:)
```PowerShell
# Muutetaan koko merkkijono isoksi
$exampleString = "tervetuloa powerShellin maailmaan!"
$capitalizedString = $exampleString.ToUpper()
Write-Output $capitalizedString
# Output: TERVETULOA POWERSHELLIN MAAILMAAN!

# Muutetaan vain jokaisen sanan ensimmäinen kirjain isoksi
$exampleString = "tervetuloa powerShellin maailmaan!"
$capitalizedWords = $exampleString -split " " | ForEach-Object { $_.Substring(0,1).ToUpper() + $_.Substring(1) } -join " "
Write-Output $capitalizedWords
# Output: Tervetuloa PowerShellin Maailmaan!
```

## Deep Dive (Syväsukellus)
Historiallisesti tekstien formaatti on ollut tapa osoittaa tyyliä tai virallisuuden astetta. PowerShellin nykyversiossa löytyvät metodit `.ToUpper()` ja `.ToLower()` ovat peräisin varhaisista ohjelmointikielistä, joissa tekstikäsittely oli olennainen osa datan käsittelyä.

Vaihtoehtoisesti stringien ensimmäisen kirjaimen muuttaminen isoksi voidaan toteuttaa myös kulttuuriherkästi käyttämällä `.ToTitleCase()` metodia, joka sijaitsee `System.Globalization.TextInfo` -luokassa. Tämä on hyödyllistä, kun kirjoitusasusäännöt vaihtelevat eri kielissä.

Implementaation näkökulmasta `.ToUpper()` ja `.ToLower()` ovat suoraviivaisia ja nopeita toimintoja, mutta eivät ota huomioon lokaalisia erikoistapauksia. Kun taas `.ToTitleCase()` pyrkii käsittelemään stringit kulttuurisesti oikein, se voi olla hitaampi ja monimutkaisempi käytössä.

## See Also (Katso Myös)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
- [Customizing String Case and Culture](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-7.0)
