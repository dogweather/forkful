---
title:                "Python: Nykyisen päivämäärän saaminen"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Miksi Päivämäärä On Tärkeä Python-Ohjelmoinnissa?

Onko sinulla koskaan ollut tarvetta tietää tämänhetkinen päivämäärä johonkin ohjelmaasi? Ehkä haluat tallentaa tietyn päivän tiedon tai suorittaa tiettyjä toimintoja vain tietyinä päivinä. Tämän vuoksi on tärkeää tietää, miten saat nykyisen päivämäärän käyttöösi Python-ohjelmassa.

## Miten Saan Nykyisen Päivämäärän Pythonilla?

Pythonilla on sisäänrakennettu datetime-kirjasto, joka sisältää toiminnon nykyisen päivämäärän ja ajan hakemiseen. Voit käyttää sitä seuraavasti:

```python
import datetime

# Tallenna nykyinen päivämäärä muuttujaan
tänään = datetime.datetime.today()

# Tulosta päivämäärä
print(tänään)

# Saat tietyn päivämäärän muodostamalla datetime-objektin
joulu = datetime.datetime(2020, 12, 25)

# Tulosta joulu
print(joulu)
```

Tämä koodi tulostaa seuraavanlaisen tuloksen:

```
2020-08-31 15:38:29.942083
2020-12-25 00:00:00
```

Voit myös muokata tulosteen muotoa lisäämällä string-formaatteja datetime-muuttujaan:

```python
# Tulosta päivä muodossa "maanantai, 31.8.2020"
print(tänään.strftime('%A, %d.%m.%Y'))
```

Tämä koodi tulostaa:

```
maanantai, 31.08.2020
```

Voit löytää lisätietoa datetime-kirjaston käytöstä [täältä](https://docs.python.org/3/library/datetime.html).

## Syvempi Sukellus

Nykyisen päivämäärän ja ajan hakeminen voi tuntua yksinkertaiselta tehtävältä, mutta datetime-kirjastolla on paljon enemmän tarjottavaa. Voit esimerkiksi käyttää sitä laskemaan ajanjaksoja ja päivämäärien välisiä eroja.

```python
# Lasketaan kuinka monta päivää on jouluun
joulu = datetime.datetime(2020, 12, 25)
ero = joulu - tänään

# Tulostetaan päivien määrä
print(ero.days)
```

Tämä koodi tulostaa:

```
116
```

Voit myös lisätä tai vähentää päiviä, tunteja ja minuutteja nykyisestä päivämäärästä käyttämällä timedelta-objektia.

## Katso Myös

- [Python datetime -virallinen dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [Datetime-opas - Real Python](https://realpython.com/python-datetime/)
- [Python päivämäärän muotoilu - GeeksforGeeks](https://www.geeksforgeeks.org/python-formatting-datetime-with-strftime-function/)