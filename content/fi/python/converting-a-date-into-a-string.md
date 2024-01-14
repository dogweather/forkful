---
title:                "Python: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Tällä blogikirjoituksella opit, miten voit muuttaa päivämäärän merkkijonoksi Python-ohjelmoinnissa. Tämä on hyödyllinen taito, kun haluat näyttää päivämäärän jossain muussa kuin alkuperäisessä muodossaan.

## Kuinka tehdä

Muuntaaksesi päivämäärän merkkijonoksi, käytä `strptime`-funktiota ja määritä haluttu muoto. Esimerkiksi:

```Python
import datetime

paivamaara = datetime.datetime(2020, 9, 25)

muoto = "%d/%m/%Y"

muutettu_paivamaara = paivamaara.strftime(muoto)

print(muutettu_paivamaara) # Tulostaa "25/09/2020"
```

Funktio `strptime` käyttää annettua muotoa päivämäärän tulkintaan ja `strftime` muuntaa päivämäärän takaisin merkkijonoksi halutussa muodossa. Voit käyttää erilaisia muotoja riippuen siitä, millaista muotoa haluat päivämäärälle.

## Syvällinen sukellus

Python tarjoaa monia vaihtoehtoja päivämäärän muuntamiseen merkkijonoksi erilaisissa muodoissa. Voit käyttää esimerkiksi `%d` merkkiä saadaksesi päivämäärän numerona, `%B` merkkiä saadaksesi kuukauden nimen kokonaisuudessaan tai `%Y` merkkiä saadaksesi vuoden nelinumeroisena. Voit myös käyttää muita merkkejä saadaksesi lisätietoja päivämäärästä, kuten viikonpäivän tai päivän etuliitteen. Lisätietoja saat Pythonin [dokumentaatiosta](https://docs.python.org/3/library/time.html#time.strftime).

## Katso myös

Tarjoamme lisätietoja Pythonin merkkijonon käsittelystä ja muuntamisesta tässä [blogikirjoituksessa](https://www.exampleblogi.fi/merkkijonon-kasittely-pythonissa). Voit myös käydä [Pythonin virallisilla verkkosivuilla](https://www.python.org/) ja liittyä [Suomen Python-yhteisöön](https://github.com/python-finland). Toivottavasti tämä kirjoitus auttoi sinua oppimaan lisää Pythonista ja sen eri toiminnoista. Onnea matkaan ohjelmoinnin maailmaan!