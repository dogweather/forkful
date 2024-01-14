---
title:                "Ruby: Laskeminen päivämäärää tulevaisuudessa tai menneisyydessä."
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Calculating date in the future or past can be a useful skill to have as a programmer. It allows you to write programs that can handle dates dynamically, making them more efficient and versatile.

## Miten

Laskeminen tulevan tai menneen päivän välillä Ruby-ohjelmoinnissa on helppoa käyttämällä Date-luokkaa. Voit käyttää seuraavia metodeja saadaksesi halutun päivämäärän:

```Ruby
Date.today # palauttaa tämän päivän päivämäärän
Date.parse("2020-12-25") # muuntaa merkkijonon päivämääräksi
```

Voit myös lisätä ja vähentää päiviä antamalla halutun määrän päiviä tai käyttämällä Date-luokan metodeja, kuten `next_day` tai `prev_day`. Tässä on esimerkki käyttämällä `next_day`-metodia:

```Ruby
Date.today.next_day(7) # palauttaa seuraavan viikon päivämäärän
```

Lopuksi, voit muuttaa päivämäärää tiettyyn aikavyöhykkeeseen käyttämällä `change`-metodia. Esimerkiksi voit muuttaa päivämäärän UTC-aikavyöhykkeestä Helsinkiin seuraavasti:

```Ruby
Date.today.change(offset: "+03:00") # muuttaa päivämäärän Helsingin aikavyöhykkeelle
```

Voit tulostaa päivämäärän haluamassasi muodossa käyttämällä `strftime`-metodia, joka hyödyntää muotoilukoodia. Alla on esimerkki, joka tulostaa päivämäärän suomalaisessa muodossa:

```Ruby
Date.today.strftime("%d.%m.%Y") # palauttaa esimerkiksi "23.09.2020"
```

## Syvempi sukellus

Date-luokassa on paljon muitakin hyödyllisiä metodeja, joten suosittelemme tutustumaan Ruby-docsin dokumentaatioon saadaksesi lisätietoja. Muista myös ottaa huomioon aikavyöhykkeet ja niiden vaikutus päivämääriin ohjelmia kirjoittaessasi.

## Katso myös

- [Ruby-docs: Date-luokka](https://ruby-doc.org/core-2.7.1/Date.html)
- [A Guide to Working with Date and Time in Ruby](https://www.rubyguides.com/2015/05/working-with-dates/)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään paremmin miten lasketaan päivämäärä tulevaisuudessa tai menneisyydessä Ruby-ohjelmoinnissa. Muista kokeilla näitä esimerkkejä ja kehittää taitojasi. Onnea ohjelmointiin!