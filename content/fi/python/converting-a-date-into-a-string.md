---
title:                "Python: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä on tarpeen muuntaa päivämäärä merkkijonoksi, jonka avulla voidaan helposti käsitellä ja näyttää päivämäärää ohjelmassa. Tässä blogikirjoituksessa käymme läpi, kuinka tämä voidaan tehdä Pythonilla.

## Kuinka tehdä

Pythonilla on helppo muuntaa päivämäärä merkkijonoksi. Se voidaan tehdä `strftime()` -funktion avulla, joka tarjoaa monia erilaisia ​​formaattivaihtoehtoja päivämäärän esittämiseksi merkkijonona.

```python
import datetime

paivamaara = datetime.datetime.now()

paivamaara_merkkijonona = paivamaara.strftime("%d-%m-%Y")

print(paivamaara_merkkijonona) # tulostaa esimerkiksi 14-07-2021
```

Tässä esimerkissä käytimme `%d` ja `%m` -merkintöjä esittämään päivämäärän päivä ja kuukausi numeroina ja `%Y` kuvaamaan vuotta neljänumeroinen luvulla. Voit kokeilla erilaisia ​​formaattivaihtoehtoja ja löytää haluamasi tavan esittää päivämäärä merkkijonona.

## Syvempi sukellus

Python-tietokanta käyttää sisäisesti datetime-objekteja päivämäärätietojen tallentamiseen. Datetime-objektin avulla voit hakea päivämäärän eri yksiköissä, kuten päivät, kuukaudet ja vuodet. Strftime-funktion avulla voit muuttaa nämä yksiköt merkkijonoksi haluamallasi tavalla.

On myös mahdollista muuntaa päivämäärä merkkijonoksi toisinpäin käyttämällä `strptime()` -funktiota. Tämä mahdollistaa päivämäärän merkkijonon muuttamisen takaisin datetime-objektiksi, joka voidaan käsitellä Pythonissa.

## Katso myös

- [Python datetime -dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [strftime() ja strptime() -esimerkkejä](https://www.programiz.com/python-programming/datetime/strftime)
- [Python dateutil -kirjasto](https://pypi.org/project/python-dateutil/)