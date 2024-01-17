---
title:                "Merkkijonon suuri kirjoitus"
html_title:           "PowerShell: Merkkijonon suuri kirjoitus"
simple_title:         "Merkkijonon suuri kirjoitus"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstijoukon/kirjoitetun merkkijonon "kohottaminen" tai "suurennustarkkuus" yksi yleisimmistä ohjelmointitehtävistä. Se tarkoittaa yksinkertaisesti merkkijonon ensimmäisen kirjaimen muuttamista isoiksi kirjaimiksi ja muiden kirjainten muuttamista pieniksi kirjaimiksi. Programmoijat tekevät tätä parantaakseen tekstin luettavuutta ja yhtenäisyyttä.

## Miten:
```PowerShell
# Esimerkkejä pääkaupungin pääkaupungin muuttamisesta Powershellissä
$String1 = "pääkaupunki"
$String1.ToUpper()

# Tulostaa: PAÄKAPUNKI

$String2 = "Suomi"
$String2.ToLower()

# Tulostaa: suomi
```

## Syvällä sukelluksella:
Pääkaupungin suurennustarkkuus on ollut osa tekstinmuokkaustyökaluja jo vuosikymmenien ajan. Ennen digitaalista aikakautta käsikirjoittajat käyttivät sanojen ja lauseiden suurennustarkkuutta parantaakseen tekstinkäsittelyvaiheita. Toinen tapa muuttaa merkkijonon kirjaimia on käyttää satuäyr-merkkiä tai sopivia funtioita. Näitä kutsutaan myös merkkijonon manipulointiin.

## Katso myös:
[Powershellin merkkijonotoiminnot](https://docs.microsoft.com/fi-fi/powershell/scripting/overview-of-string-manipulation?view=powershell-7.1)