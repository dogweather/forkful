---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Nykypäivän hankkiminen ohjelmassa tarkoittaa nykyisen päiväyksen ja/tai ajan palauttamista. Ohjelmoijat tarvitsevat sitä usein aikaleimoja, ajastimia tai jopa kalenteriominaisuuksia varten.

## Miten tehdään:

Pythonissa voit käyttää `datetime` kirjastoa nykyisen päivän ja ajan saamiseen. Tässä on yksinkertainen esimerkki siitä, miten se tehdään:

```Python
from datetime import datetime

nykyinen_paiva = datetime.now()
print(nykyinen_paiva)
```

Kun suoritat tämän koodin, se palauttaa nykyisen päivämäärän ja ajan muodossa 'Vuosi-Kuukausi-Päivä Tun:Min:Sec.Millisec', esimerkiksi `2023-09-24 13:45:22.914617`.

## Syvällinen tieto

Historiallisessa kontekstissa, aikaisemmissa ohjelmointikielissä saattoi olla paljon monimutkaisempaa saada nykyinen päivämäärä ja aika. Pythonin syntaksi on suunniteltu olemaan luettavampi ja helpompi ymmärtää, mikä tekee tällaisista tehtävistä helpompia.

Alternativejakin toki on, esim. 'time'-kirjasto:

```Python
import time

nykyinen_aika = time.ctime()
print(nykyinen_aika)
```

Tämä palauttaa nykyisen ajan ihmismuotoisessa muodossa (esimerkiksi 'Tue Jun 29 14:05:28 2023').

Näissä koodiesimerkeissä 'datetime' ja 'time' kirjastot palauttavat ajan tietokonejrjestelmän kellosta, joka on yleensä synkronoitu verkon yli.

## Katso myös

- Pythonin virallinen dokumentaatio: [datetime](https://docs.python.org/3/library/datetime.html) ja [time](https://docs.python.org/3/library/time.html)
- [Python aloittelijoiden opas](https://www.python.org/about/gettingstarted/)
- [Redditin Python-yhteisö](https://www.reddit.com/r/Python/), mainio paikka saada kysymyksiin vastauksia.

Muista, tärkeimmästä tähän ja muihin ohjelmoinnin perusteisiin voi löytyä useita ratkaisuja - usein parasta on se, joka on ymmärrettävin itsellesi tai tiimillesi.