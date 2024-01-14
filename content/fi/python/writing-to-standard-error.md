---
title:    "Python: Kirjoittaminen standardivirheeseen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standard erroriin?

Kirjoittaminen standard erroriin on tärkeä osa Python-ohjelmointia, sillä se mahdollistaa virheiden hallinnan ja ohjelman sujuvan toiminnan. Näin voit tunnistaa ja korjata ongelmia koodissasi ja tehdä siitä entistä tehokkaampaa.

## Kuinka kirjoittaa standard erroriin

```Python
try:
    # Tähän kirjoitat koodisi, joka saattaa aiheuttaa virheitä
except Exception as e:
    # Tässä voit kirjoittaa virheen tiedot standard erroriin
    print("Virhe: ", e, file=sys.stderr)
```

Koodiblokin sisällä oleva `try-except`-rakenne mahdollistaa ohjelman suorituksen jatkumisen, vaikka koodin sisältämässä lohkossa tapahtuisi virhe. `except`-lohkon `print`-komento kirjoittaa virheen tiedot standard erroriin, jotta ne voidaan myöhemmin tarkastella ja korjata.

## Syvällisempi tarkastelu

Standard error on yksi tapa hallita virheitä Pythonissa. Sitä käytetään yleensä yhdessä `try-except`-rakenteen kanssa, mutta voi myös kirjoittaa suoraan virheen tiedot `sys.stderr`-muuttujaan. Tämä auttaa säilyttämään koodin suorituksen hallinnassa ja välttää näkymättömiä virheitä.

## Katso myös
- [Pythonin käyttöönotto]() - opas Pythonin asentamiseen ja käynnistykseen
- [Pythonin virheiden hallinta]() - lisätietoja virheiden hallinnasta Pythonissa.