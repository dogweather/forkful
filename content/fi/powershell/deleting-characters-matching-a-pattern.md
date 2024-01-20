---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Hahmojen poistaminen kuvion mukaan on prosessi, jossa kaikki halutun kaavan mukaiset hahmot poistetaan merkkijonosta. Ohjelmoijat tekevät tämän tiedon puhdistamiseen ja muotoilun yksinkertaistamiseen.

## Kuinka:
Käytämme `-replace` operaattoria. Seuraavassa esimerkissä on poistettu kaikki numerot merkkijonosta:

```PowerShell
$myString = "abc123def456ghi789"
$myString = $myString -replace '\d',''
$myString
```

Tämän koodin tulostus on: `abcdefghi`

## Syvempi sukellus:
PowerShellin `-replace` operaattori hyödyntää .NET:in säännöllisen lausekkeen moottoria. Se luotiin 1990-luvulla ja on ollut peruspilari monissa ohjelmointikielissä.

On olemassa vaihtoehtoja, kuten `-split` ja `-join` operaattorit:

```PowerShell
$myString = "abc123def456ghi789"
$myString = ($myString -split '\d') -join ''
$myString
```

Tämäkin tuottaa `abcdefghi`.

Hahmojen poistamiseen kuvion mukaan PowerShellissa on siis kaksi tapaa: `-replace` ja `-split` tyyppiset operaattorit. Kumpi käyttää riippuu sovellustapauksesta.

## Katso myös:
- PowerShellin dokumentaatio `-replace` operaattorista: https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#replacement-operator
- Microsoftin .NET tuki säännöllisille lausekkeille: https://docs.microsoft.com/fi-fi/dotnet/standard/base-types/regular-expressions
- PowerShellin dokumentaatio `-split` operaattorista: https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7.1