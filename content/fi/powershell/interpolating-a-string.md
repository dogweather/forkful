---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Interpoloiminen on tapa muotoilla merkkijonoja yhdistämällä staattisia merkkijonoja ja dynaamisia arvoja sujuvasti yhdeksi merkkijonoksi. Ohjelmoijat käyttävät sitä saadakseen selkeämmän koodin ja välttääksensä merkkijonojen yhdistämisen virheet.

## Kuinka tehdä:

Katsotaan esimerkkiä:

```PowerShell
$hahmo = "Kissa"
$toiminta = "hypätä"
Write-Output "Näen $hahmo:n, joka yrittää $toiminta:a."
```

Tämän suorittaminen tulostaa tuloksen:

"Näen Kissa:n, joka yrittää hypätä:a."

Stringin interpolointi PowerShellissä käyttää $-merkkiä muuttujien nimeämiseen merkkijonon sisällä.

## Syventävä sukellus:

Historiallisesti merkkijonojen yhdistämistä varten on ollut useita metodeja, kuten + -operaattorin tai `String.Format`-metodin käyttö. Nämä voivat kuitenkin olla kömpelöitä ja johtaa virheisiin. Stringin interpolointi PowerShellissä tarjoaa selkeämmän ja vähemmän virhealttiin tavan.

Alternatiiveille, `StringBuilder` on varteenotettava vaihtoehto suurille merkkijonoille, ja `-f` -operaattorille, joka on samanlainen kuin `String.Format`.

PowerShellissä merkkijonojen interpolointi käyttää kaksoislainauksia (""). Yksittäiset lainaukset ('') eivät toimi, koska ne käsittävät merkkijonon kirjaimellisesti.

## Katso myös:

[Microsoftin dokumentaatio stringin interpoloinnista](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.1)

[Keskustelu stringin interpoloinnista StackOverflow:issa](https://stackoverflow.com/questions/37320296/string-interpolation-vs-format-string-in-powershell)