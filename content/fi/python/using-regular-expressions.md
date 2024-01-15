---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Python: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat tehokas työkalu tekstin käsittelyssä ja etsimisessä. Niiden avulla voit suorittaa monimutkaisia hakuja ja korvauksia tekstissä, mikä säästää aikaa ja vaivaa monissa ohjelmointitilanteissa.

## Kuinka käyttää säännöllisiä lausekkeita?

Säännöllisiä lausekkeita käytetään Pythonin `re` -kirjaston avulla. Ensimmäiseksi sinun on tuotava `re` -kirjasto:

```Python
import re
```

Sitten voit käyttää `re` -kirjaston eri funktioita säännöllisten lausekkeiden luomiseen. Esimerkiksi voit käyttää `re.search()` -funktiota etsimään tietyn kaavan mukaisia merkkijonoja. Tässä on yksinkertainen esimerkki, jossa etsitään kaikkia sanoja, jotka alkavat kirjaimella "h" ja loppuvat kirjainpariin "en":

```Python
teksti = "Hei, tämä on esimerkki hakusanoista, jotka alkavat h:lla ja loppuvat en:aan"
hakutulos = re.search(r"h\w+en", teksti)
print(hakutulos.group())
```

Tämä tulostaa "hakusanojen" sijasta "haku" osoituksena siitä, että säännölliset lausekkeet ovat löytäneet oikean vastineen tekstissä.

## Syvennä tietämystäsi säännöllisistä lausekkeista

Säännölliset lausekkeet voivat tuntua aluksi vaikeilta, mutta niiden avulla voit suorittaa monimutkaisia hakuja ja korvauksia tekstissä. Voit lukea lisää säännöllisistä lausekkeista Pythonin virallisesta dokumentaatiosta tai käyttää online-selaimia, kuten Regex101, harjoitellaksesi ja testataksesi erilaisia säännöllisiä lausekkeita.

## Katso myös

- [Pythonin virallinen dokumentaatio säännöllisistä lausekkeista](https://docs.python.org/3/library/re.html)
- [Regex101](https://regex101.com/) - online-selain säännöllisten lausekkeiden harjoitteluun ja testaamiseen
- [Säännöllisten lausekkeiden opas](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-regex-in-python-3) - oppimateriaali säännöllisistä lausekkeista Pythonissa