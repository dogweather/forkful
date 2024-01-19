---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Mielenkiintoista PowerShellin (nykyinen versio) ohjelmointia: Merkkijonojen yhdistäminen

## Mitä & Miksi?

Merkkijonojen yhdistäminen ('concatenation') on prosessi, jossa kaksi tai useampia merkkijonoja liitetään yhteen yhdeksi merkkijonoksi. Ohjelmoijat käyttävät sitä luodakseen dynaamisia merkkijonoja tai korjaamaan merkkijonojen liittyviä ongelmia.

## Miten:

Tässä on nopea esimerkkiskripti, joka näyttää, kuinka yhdistät merkkijonot PowerShellissa:

```PowerShell
$string1 = "Hei"
$string2 = " maailma!"

$joinedString = $string1 + $string2
Write-Host $joinedString
```

Sample output:
```PowerShell
Hei maailma!
```

Tässä toinen esimerkki, joka käyttää -f -muotomäärittäjää:

```PowerShell
$template = "Hei {0}!"
$user = "maailma"
$greeting = $template -f $user

Write-Host $greeting
```

Sample output:
```PowerShell
Hei maailma!
```

## Syvällisempää: 

Historiallisesti tuetaan merkkijonojen yhdistämistä jo useissa vanhemmissakin ohjelmointikielissä. PowerShell on ottanut parhaita käytäntöjä näistä ja tarjoaa useita erityisiä keinoja.

Vaihtoehtoisesti voit käyttää -join-komentoa:

```PowerShell
$string1 = "Hei"
$string2 = " maailma!"
$joinedString = $string1, $string2 -join ''

Write-Host $joinedString
```

Yhdistämällä merkkijonot joissakin tapauksissa voi aiheuttaa suorituskyvyn pudotuksen. Suuren määrän merkkijonojen liittämisessä kannattaa käyttää StringBuilder-luokkaa, joka on suunniteltu tätä erityistä tarkoitusta varten.

## Katso myös:

Merkkijonojen y concatentation .NET:ssä: https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings

PowerShellin -f -muotomäärittäjä: https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-string-substitutions?view=powershell-7.1

StringBuilder-luokka: https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder.append?view=net-5.0