---
title:                "Päivämäärän erottelu merkkijonosta"
html_title:           "Python: Päivämäärän erottelu merkkijonosta"
simple_title:         "Päivämäärän erottelu merkkijonosta"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän erottaminen merkkijonosta on prosessi, jossa päivämääräyksiköt tunnistetaan ja purkautuvat merkkijonosta. Tätä tehdään yleensä joko tiedon tallentamiseksi tai sen muuttamiseksi halutussa muodossa. Esimerkiksi kun käyttäjä syöttää päivämäärän tekstikenttään, se täytyy muuttaa tietokoneen ymmärtämään muotoon.

## Kuinka tehdä:
Näin tehdään päivämäärän erottaminen merkkijonosta Pythonissa:

```Python
from datetime import datetime
date_string = "12.5.2020"
date = datetime.strptime(date_string, "%d.%m.%Y")
print(date)
```

Tuloste: 2020-05-12 00:00:00

## Syvemmälle:
Päivämäärän erottaminen merkkijonosta ei ole uusi konsepti, ja sitä on tehty jo vuosikymmeniä. Alkuaikoina sitä tehtiin usein manuaalisesti käsin koodaamalla, mutta nyttemmin siihen löytyy monia valmiita kirjastoja Pythonissa, kuten datetime-moduuli. On myös olemassa muita tapoja tehdä päivämäärän erottaminen, kuten käyttämällä säännöllisiä lausekkeita, mutta datetime-moduuli on yleensä suositumpi vaihtoehto sen yksinkertaisemman käytön vuoksi.

## Katso myös:
- [Pythonin datetime-moduuli](https://docs.python.org/3/library/datetime.html)
- [Säännölliset lausekkeet Pythonissa](https://docs.python.org/3/howto/regex.html)