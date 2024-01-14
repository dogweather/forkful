---
title:    "Python: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Jos joskus olet halunnut tietää millainen päivämäärä on esimerkiksi kolme viikkoa tulevaisuudessa tai menneisyydessä, niin tämä blogipostaus on juuri sinulle! Opit kuinka voit laskea päivämääriä Python-ohjelmoinnin avulla.

## Kuinka tehdä

Käytä tätä koodiesimerkkiä laskeaksesi päivämäärän esimerkiksi 2 viikkoa eteenpäin:

```Python
import datetime

tänään = datetime.date.today()
muutettu_päivä = tänään + datetime.timedelta(days=14)

print(muutettu_päivä.strftime("%d/%m/%Y"))
```

Tämä koodi käyttää datetimen "today" -toimintoa määrittämään tämänhetkisen päivämäärän ja lisää siihen timedelta-funktion avulla kaksi viikkoa. Lopulta "strftime" -toiminto muuttaa päivämäärän muodon halutuksi.

Tässä on esimerkki tulosteesta:

```
17/06/2021
```

Voit myös käyttää samaa koodia laskettaessa päivämäärää menneisyydessä. Vaihda vain timedelta-funktion päiviä negatiiviseksi luvuksi haluamasi päivien määrän perusteella.

## Syvä sukellus

Jos haluat tietää lisää datetimien ja timedelta-funktion käytöstä, suosittelemme tutustumaan Pythonin viralliseen dokumentaatioon. Sieltä löytyy kattava tietopaketti näiden toimintojen käytöstä ja mahdollisuuksista.

## Katso myös

- [Pythonin virallinen dokumentaatio datetime-toiminnoista](https://docs.python.org/3/library/datetime.html)
- [DateTime -toimintojen käyttö takautuvasti](https://www.saltycrane.com/blog/2008/06/how-to-find-date-time-difference-in/)
- [Laskuri eri aikavyöhykkeiden välillä](https://timeanddate.com)