---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "PowerShell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Säännölliset lausekkeet ovat erilaisia ilmaisuja, joita käytetään tekstien ja merkkijonojen hakuun ja muokkaamiseen. Ohjelmoijat käyttävät niitä yleisesti, koska ne ovat tehokas tapa löytää ja työstää tiettyjä osia tekstistä.

# Miten:
Koodiesimerkit ja tulosteesi:

```PowerShell
# Esimerkki 1 - Merkin etsiminen
$text = "Tämä on esimerkki tekstistä."
if ($text -match "esimerkki") {
    Write-Host "Löydettiin merkkijono 'esimerkki' tekstistä."
} else {
    Write-Host "Merkkijonoa 'esimerkki' ei löytynyt tekstistä."
}

# Esimerkki 2 - Numerojen poimiminen
$numbers = Get-Content .\numerot.txt
$pattern = "\d+" # Hakee kaikki numerot
$matches = [regex]::Matches($numbers, $pattern)
foreach ($match in $matches) {
    Write-Host $match.Value # Tulostaa löydetyn numeron
}
```

Tuloste:

```PowerShell
Esimerkki 1 - Merkin etsiminen
Löydettiin merkkijono 'esimerkki' tekstistä.

Esimerkki 2 - Numerojen poimiminen
123
456
789
```

# Syväsukellus:
Säännölliset lausekkeet kehitettiin 1950-luvulla matematiikan ja laskennan alueella, mutta niitä alettiin pian käyttää myös tiedonkäsittelyssä ja ohjelmoinnissa. Niiden avulla voidaan säästää aikaa ja vaivaa, kun etsitään ja muokataan tiettyjä merkkijonoja. Vaihtoehtoja säännöllisille lausekkeille ovat esimerkiksi merkkijonojen etsimiseen ja muokkaamiseen tarkoitetut funktiot, mutta säännölliset lausekkeet ovat yleensä tehokkaampi ja monipuolisempi vaihtoehto.

Säännöllisten lausekkeiden toteutus PowerShellissä perustuu .NET Frameworkin System.Text.RegularExpressions-luokkaan. Tämä luokka tarjoaa useita hyödyllisiä ominaisuuksia, kuten kyvyn poimia säännöllisillä lausekkeilla määrättyjä osia merkkijonosta.

# Katso myös:
- [Säännöllisten lausekkeiden opas (W3Schools)](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)