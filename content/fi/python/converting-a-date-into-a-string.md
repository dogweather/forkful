---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Muunnos päivämäärästä merkkijonoksi Pythonissa tarkoittaa päivämäärän muuttamista luettavaksi tekstiksi - merkkijonoksi. Tätä tarvitaan useimmiten tiedon näyttämisessä selkeästi käyttäjälle tai päivämäärien tallennukseen tekstimuodossa.

## Näin Se Teetään

Pythonin sisäänrakennetun `datetime`-moduulin avulla muuntaminen on helppoa. Tässä on esimerkki siitä, miten se toimii:

```Python
from datetime import datetime
nyt = datetime.now()
merkkijonona = nyt.strftime("%m/%d/%Y, %H:%M:%S")

print(merkkijonona)
```

Tämä koodi tuottaa seuraavanlaisen tulosteen nykyisellä ajalla:

```Python
"02/23/2023, 14:30:10"
```

`strftime()`-funktio saa argumentiksi muotoiluohjeet, joilla määritellään tulosteen muoto.

## Deep Dive

### Historiaa

`strftime()`-funktion nimi tulee C-ohjelmointikielestä, jossa "s" tarkoittaa merkkijonoa (string) ja "f" tarkoittaa muotoilua (format). Pythonissa tämä funktio on otettu käyttöön sen vanhoista versioista lähtien ja se on yleisesti käytetty tapa päivämäärän muuttamiseen merkkijonoksi.

### Vaihtoehdot

Voit myös käyttää päivämäärän suoraan merkkijonona, mutta tällöin menetät mahdollisuuden tehdä päivämäärien välisiä laskutoimituksia vaivattomasti. Muut Python kirjastot, kuten `pendulum` tai `arrow`, tarjoavat myös päivämäärän muuntamisen merkkijonoksi, mutta ne ovat vähemmän tunnettuja ja niitä käytetään harvemmin.

### Toteutus

`strftime`-metodi on osa Pythonin `datetime`-moduulia, joka käyttää C-ohjelmointikielen ajan funktioita.

## Katso Myös

['datetime' Pythonin dokumentaatiossa](https://docs.python.org/fi/3/library/datetime.html)

['strftime()' ja 'strptime()' käyttö Pythonissa](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) 

['pendulum' kirjaston kotisivut](https://pendulum.eustace.io/)

['arrow' kirjaston kotisivut](https://arrow.readthedocs.io/)