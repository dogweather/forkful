---
title:                "Merkkijonosta lainausmerkkien poistaminen"
date:                  2024-01-26T03:41:08.265991-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Lainausmerkkien poistaminen merkkijonosta PowerShellissä poistaa yksittäiset (`'`) tai kaksinkertaiset (`"`) lainausmerkit tekstin ympäriltä. Ohjelmoijat tarvitsevat usein puhdistaa merkkijonoja käsittelyä, vertailua tai tulostusta varten, erityisesti käsiteltäessä käyttäjän syötettä tai tiedoston jäsentämistä.

## Kuinka:
Voit käyttää `-replace`-operaattoria poistaaksesi lainausmerkit merkkijonosta. Näin se tapahtuu:

```PowerShell
# Korvaa yksittäiset lainausmerkit
$stringWithSingleQuotes = "'Hello, World!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Tuloste: Hello, World!

# Korvaa kaksinkertaiset lainausmerkit
$stringWithDoubleQuotes = '"Hello, World!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Tuloste: Hello, World!
```

Molemmille tyypeille:

```PowerShell
$stringWithQuotes = '"Hi there," she said.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Huomaa regex-luokkamerkkien käyttö
Write-Output $cleanString  # Tuloste: Hi there, she said.
```

Konsolin näyteulostus näyttää jotakuinkin tältä:

```
Hello, World!
Hello, World!
Hi there, she said.
```

## Syväsukellus
Ennen PowerShellin aikaa, kun se oli vasta pilke Microsoftin silmäkulmassa, tekstinkäsittely Windowsissa oli usein erja-skriptien aluetta, joilla oli rajoitettuja ominaisuuksia. PowerShellin esittely toi mukanaan tehokkaita merkkijononkäsittelyominaisuuksia, jotka tekivät skriptauksesta paljon monipuolisempaa.

Vaihtoehtoja `-replace`:lle on olemassa, kuten `.Trim()`-metodin käyttäminen lainausmerkkien poistamiseen vain merkkijonon alusta ja lopusta, mutta ne eivät tarjoa samaa hallintaa tai regex-tukea.

```PowerShell
# Käyttäen .Trim():ia lainausmerkkien poistoon alusta ja lopusta
$stringWithQuotes = '"Hello, World!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Tuloste: Hello, World!
```

Huomaa, että `-replace` käyttää regexiä taustalla, joten kun työskentelet sen kanssa, pidä mielessä, että erikoismerkit on pakotettava, jos olet kohdentamassa niitä. Jos tarvitset tarkempaa hallintaa lainausmerkkien poistossa, syventyminen regexiin `-replace`-avulla on tie eteenpäin, antaen sinulle valtavan joustavuuden.

## Katso myös
- Lisätietoja regexistä PowerShellissä, tarkista viralliset dokumentit: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Tutustu merkkijonon käsittelyn syvyyksiin: [about_String](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
- Löydä muita merkkijonometodeja: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)