---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
"## Mikä & Miksi?"

Säännölliset lausekkeet, regex, ovat tekstinhakupatterneja. Ne auttavat löytämään, korvaamaan ja validoidaan tietoa nopeasti.

## How to:
"## Kuinka tehdä:"

Esimerkki: Hae kaikki email-osoitteet merkkijonosta.

```PowerShell
$text = 'jukka@mehilainen.fi lorem ipsum petri@terveydeksi.fi'
$pattern = '[\w\.-]+@[\w\.-]+\.\w+'
[regex]::Matches($text, $pattern) | ForEach-Object { $_.Value }
```

Tulostus:

```
jukka@mehilainen.fi
petri@terveydeksi.fi
```

## Deep Dive
"## Syväsukellus"

Historia: Regex syntyi 1950-luvulla. Nykyään se on ohjelmoinnin peruskauraa. Vaihtoehtoja regexille on kuten String haku, mutta ne ovat hitaampia ja vähemmän monipuolisia. Toteutus: PowerShell käyttää .NET regex -luokkaa, joka tarjoaa vankan toteutuksen.

## See Also
"## Katso myös"

- [Microsoftin regex-opas](https://docs.microsoft.com/fi-fi/dotnet/standard/base-types/regular-expressions)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [PowerShellin about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.2)
