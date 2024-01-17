---
title:                "Tulevan tai menneen päivämäärän laskeminen"
html_title:           "Fish Shell: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Päivämäärien laskeminen tulevaisuutta tai menneisyyttä varten tarkoittaa tietyn päivämäärän lisäämistä tai vähentämistä tietyn määrän päiviä. Ohjelmoijat tekevät tätä esimerkiksi laskiessaan tulevaa tapahtumaa tai tarkistaessaan ajastettujen tehtävien päivämääriä.

# Miten?

```Fish Shell```-ohjelman avulla voit helposti laskea tulevaisuuden tai menneisyyden päivämääriä. Alla olevassa esimerkissä lisätään 30 päivää nykyiseen päivämäärään ja tuloksena saadaan 2.tammikuuta 2021.

```
Fish Shell date -d "+30 days" +%d.%m.%Y
02.01.2021
```

Voit myös lisätä päiviä tai kuukausia suoraan haluamaasi päivämäärään, kuten seuraavassa esimerkissä, jossa lisätään 3 kuukautta 15 päivää 9.huhtikuuta 2020:

```
Fish Shell date -d "09.04.2020 +3 months 15 days"
24.07.2020
```

# Syvemmälle

Päivämäärien laskeminen on yleinen tehtävä ohjelmoinnissa ja tästä syystä siihen löytyy useita eri tapoja. ```Fish Shell``` tarjoaa kuitenkin helpon ja nopean tavan laskea päivämääriä, joten sitä kannattaa ehdottomasti hyödyntää.

## Vaihtoehtoja

On olemassa monia muita ```Shell```-ohjelmia ja kielitietoja, jotka tarjoavat samanlaisia toimintoja päivämäärien laskemiseen, kuten esimerkiksi ```Bash``` ja ```Python```. Voit myös käyttää erilaisia tietokoneen kalenterisovelluksia, joissa on sisäänrakennettu päivämäärien laskenta-toiminto.

## Taustatietoa

Päivämäärien laskenta on tärkeä osa ohjelmointia, sillä se auttaa ohjelmoijia aikatauluttamaan tehtäviä ja hallitsemaan aikaa. Alun perin päivämäärien laskenta perustui maailmanlaajuiseen standardiin nimeltä Unix Time, joka määrittää päivämäärät sekunteina tietystä ajankohdasta lähtien.

# Katso myös

[Täältä](https://fishshell.com/docs/current/index.html) löydät lisätietoa ```Fish Shell```ista ja sen eri toiminnoista, mukaan lukien päivämäärien laskeminen. Voit myös tutustua erilaisiin päivämäärien laskentamenetelmiin ja niiden käyttöön eri ohjelmointikielillä.