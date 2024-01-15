---
title:                "Kahden päivämäärän vertailu"
html_title:           "Fish Shell: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Vertaileminen on yksi tehokkaimmista tavoista käsitellä tietoa ohjelmoinnissa. Kaksien päivämäärän vertaileminen mahdollistaa tietyn ajanjakson tai aikavälin tutkimisen, mikä voi olla hyödyllistä esimerkiksi raportoinnissa tai tietokantojen käsittelyssä.

## Miten vertailla päivämääriä Fish Shell -ohjelmassa?

Fish Shell tarjoaa kätevän tavan vertailla kahta päivämäärää. Käyttämällä "date" -komentoa ja "-s" ja "-t" vaihtoehtoja voit vertailla kahta päivämäärää haluamallasi tavalla. Alla on esimerkki koodipalasta, jossa ensimmäinen päivämäärä on nykyinen päivä ja toinen päivämäärä on 7 päivää eteenpäin. 

```Fish Shell
date -s -7d; date -s; date -t
```
Tämän komennon tulosteessa näet ensimmäisen päivämäärän olevan 7 päivää nykyisen päivän edellä ja toisen päivämäärän olevan nykyinen päivä.

## Syvempää tietoa

Vertaaminen vaatii hieman ymmärrystä päivämäärien muotoilusta. Fish Shellissa päivämäärät esitetään Unix-timestamp-muodossa, joka ilmaisee sekunteina kuluneen ajan vuodesta 1970 tiettyyn päivämäärään. Tämä mahdollistaa päivämäärien helpon vertailun, koska suurempi timestamp tarkoittaa myös myöhemmällä olevaa päivämäärää.

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/cmds/date.html)
- [Vertaile kaksi päivämäärää Shell Bashissa](https://linuxhint.com/compare-dates-bash/)