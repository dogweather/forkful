---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "PowerShell: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lasketaan päivämäärä tulevaisuudessa tai menneisyydessä tarkoittaa ajankohdan laskemista tietyn ajanjakson päästä tai ennen nykyistä hetkeä. Ohjelmoijat tekevät sen tiettyjen tehtävien ajoittamiseksi tai aikarajojen hallinnan helpottamiseksi.

## Miten:

```PowerShell
#Lasketaan päivämäärä 30 päivän päästä
$paivamaara = Get-Date
$paivamaara.AddDays(30)

#Lasketaan päivämäärä 7 päivää sitten
$paivamaara = Get-Date
$paivamaara.AddDays(-7)
```

Esimerkkilähtö:

```PowerShell
Torstai, 21. tammikuuta 2022 13:48:26
Tiistai, 14. tammikuuta 2022 13:48:26
```

## Syvällinen tarkastelu

- Historiallinen yhteys: PowerShell on ollut käytettävissä vuodesta 2006, ja se on korvannut vanhemmat Windows-komentosarjakohtaiset työkalut.
   
- Vaihtoehdot: Voit käyttää muitakin ohjelmointikieliä tulevien tai menneiden päivämäärien laskemiseen, esimerkiksi Pythonissa käyttämällä datetime-moduulia.
   
- Toteutuksen yksityiskohdat: "Get-Date"-komento palauttaa nykyisen päivämäärän ja kellonajan, ja "AddDays"-metodi lisää tai vähentää annetun määrän päiviä.

## Katso myös 

1. Microsoft PowerShell Documentation: [https://docs.microsoft.com/is-fi/powershell/](https://docs.microsoft.com/is-fi/powershell/)
2. Python DateTime Module: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
  
Jotta voit parantaa taitojasi päivämäärien ja ajan manipuloinnissa, suosittelemme, että tutustut myös muiden ohjelmointikielten dokumentaatioon.