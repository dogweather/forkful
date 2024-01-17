---
title:                "Hankkimassa nykyistä päivämäärää"
html_title:           "Python: Hankkimassa nykyistä päivämäärää"
simple_title:         "Hankkimassa nykyistä päivämäärää"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

"Päivämäärän hankkiminen" tarkoittaa nykyisen päivämäärän ja ajan selvittämistä. Ohjelmoijat tekevät tätä esimerkiksi aikaleimojen lisäämiseksi tietokantaan tai tiedostoihin.

## Kuinka:

```Python
import datetime

nykyinen_paivamaara = datetime.datetime.now()

print("Nykyinen päivämäärä ja aika:", nykyinen_paivamaara)
```

```Python
# Tulos:

Nykyinen päivämäärä ja aika: 2020-12-09 09:30:00.000000
```

## Syväsukellus:

Päivämäärän ja ajan hankkiminen on yksi perusoletustoiminnoista Pythonissa. Tätä varten käytetään datetime-moduulia, jonka avulla voidaan luoda objekti, joka sisältää nykyisen päivämäärän ja ajan. Tätä objektia voidaan sitten käyttää esimerkiksi erilaisissa laskutoimituksissa ja muissa toiminnoissa.

### Historiallinen konteksti:

Aikaisemmissa Pythonin versioissa (ennen versiota 2.3) oli mahdollista käyttää time-moduulia nykyisen ajan ja päivämäärän hankkimiseen. Time-moduuli kuitenkin palauttaa arvonaan tarkkaa aikaa Unix epoch -muodossa, eli sekunteina 1. tammikuuta 1970:stä lähtien. Nykyään datetime-moduuli on suositeltu tapa hankkia nykyinen päivämäärä ja aika, koska se antaa käyttäjälle selkeämmän ja helpommin muokattavan objektin päivämäärälle ja ajalle.

### Vaihtoehtoiset tavat:

Pythonin lisäksi useimmissa käyttöjärjestelmissä ja ohjelmointikielissä on valmiina mahdollisuus hankkia nykyinen päivämäärä ja aika. Esimerkiksi Unix-käyttöjärjestelmässä käytetään "date" -komennolla ja Java-kielellä Date-luokalla.

### Toteutus:

Datetime-moduulin toiminta perustuu ajanjaksoon, joka on kulunut Unix epochista nykyhetkeen. Moduuli käyttää tätä ajanjaksoa ja siitä laskettua aikaleimaa selvittämään nykyisen päivämäärän ja ajan.

## Katso myös:

- [Datetime-moduuli Pythonin virallisessa dokumentaatiossa](https://docs.python.org/3/library/datetime.html)
- [Time-moduuli Pythonin virallisessa dokumentaatiossa](https://docs.python.org/3/library/time.html)