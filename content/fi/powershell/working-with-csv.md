---
title:                "Ohjelmointi csv-tiedostojen kanssa"
html_title:           "PowerShell: Ohjelmointi csv-tiedostojen kanssa"
simple_title:         "Ohjelmointi csv-tiedostojen kanssa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
CSV (Comma-Separated Values) on taulukkodataformaatti, jota voidaan käyttää tietojen tallentamiseen ja jakamiseen. CSV-tiedostot ovat suosittuja ohjelmoijien keskuudessa, koska ne ovat helppoja luoda ja käsitellä. Ohjelmoijat käyttävät CSV-tiedostoja tavallisesti tietokantojen, taulukoiden tai muiden rakenteiden tuomiseen ja viemiseen.

## Kuinka tehdä:
PowerShell tarjoaa useita tapoja käsitellä CSV-tiedostoja. Yksinkertaisin tapa on käyttää ```Import-Csv``` komentoa, joka lataa CSV-tiedoston muistiin ja muuttaa sen PowerShell-objekteiksi.

```
PowerShell> $csvData = Import-Csv -Path C:\Users\kayttaja\Documents\data.csv
```
Nyt voit käyttää `$csvData` muuttujaa ja sen sisältämiä tietoja kuten mitä tahansa muuta PowerShell-objektia.

Toinen tapa on käyttää ```Export-Csv``` komentoa, joka muuttaa PowerShell-objektit takaisin CSV-tiedostoksi.

```
PowerShell> $data = Get-Process
PowerShell> $data | Export-Csv -Path C:\Users\kayttaja\Documents\processes.csv
```
Tämä esimerkki tallentaa tietokoneesi käynnissä olevien prosessien tiedot CSV-tiedostoksi.

## Syvällinen sukellus:
CSV-muoto luotiin alun perin tietokoneohjelmistoissa käytettäväksi, joten se on erittäin yhteensopiva tietokantajärjestelmien ja verkkosovellusten kanssa. Verrattuna muihin tiedostomuotoihin, CSV-tiedostot ovat myös suhteellisen pienikokoisia, mikä tekee niistä ihanteellisia datan jakamiseen.

Vaikka PowerShellin sisäänrakennetut komennot ovat hyödyllisiä CSV-tiedostojen käsittelyssä, on myös muita vaihtoehtoja, kuten käyttää erityisiä CSV-moduuleja tai työkaluja.

CSV-tiedostojen tyypillinen rakenne koostuu sarakkeista ja riveistä, joissa jokainen rivi vastaa yhtä tietuetta ja sarakkeet ovat tietojen ominaisuuksia. On tärkeää ottaa huomioon, että CSV-tiedostot eivät tue monimutkaisia tietotyyppejä, kuten taulukoita tai objekteja.

## Katso myös:
- [PowerShell CSV-dokumentaatio](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [PowerShell CSV-tutoriaali](https://www.youtube.com/watch?v=2lNx33fH_pM)