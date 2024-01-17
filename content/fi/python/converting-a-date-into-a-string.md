---
title:                "Muunna päivämäärä merkkijonoksi."
html_title:           "Python: Muunna päivämäärä merkkijonoksi."
simple_title:         "Muunna päivämäärä merkkijonoksi."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämäärän esittämistä tekstimuodossa. Ohjelmoijat tekevät tämän tarpeen mukaan, kuten tallentaessaan päivämäärää tietokantaan tai näyttääkseen päivämäärän käyttäjälle ohjelman käytön aikana.

## Kuinka:

```Python
import datetime

# Muunna päivämäärä merkkijonoksi
paivamaara = datetime.datetime.now().strftime("%d-%m-%Y %H:%M:%S")
print(paivamaara) # Tulostaa nykyisen päivämäärän ja ajan

# Muunna merkkijono takaisin päivämääräksi
muunnettu_paivamaara = datetime.datetime.strptime("30-07-2020 23:55", "%d-%m-%Y %H:%M")
print(muunnettu_paivamaara) # Tulostaa muunnetun päivämäärän

# Tulostaa päivämäärän halutussa muodossa
print(paivamaara.strftime("%A, %d %B %Y")) # Tulostaa nykyisen päivämäärän muodossa "Torstai, 30 Heinäkuu 2020"
```

## Syvällinen sukellus:

Päivämäärän muuntaminen merkkijonoksi on ollut tarpeellista tietokoneohjelmoinnin aikaisista vuosista lähtien. Se auttaa ohjelmoijia tallentamaan päivämääriä tietokantaan käyttäen vähemmän tallennustilaa ja mahdollistaa päivämäärän esittämisen käyttäjille helpommin luettavassa muodossa.

Toinen tapa muuttaa päivämäärä merkkijonoksi on käyttää `strftime()` -funktiota, joka mahdollistaa päivämäärän muotoilun halutulla tavalla. Lisäksi `strptime()` -funktio mahdollistaa merkkijonon muuntamisen takaisin päivämääräksi antamalla oikean muotoilun.

## Katso myös:

[Tietoa Pythonin päivämäärä- ja aikafunktioista](https://docs.python.org/3/library/datetime.html)