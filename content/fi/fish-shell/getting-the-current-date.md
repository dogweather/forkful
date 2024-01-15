---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Fish Shell: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi saada nykyinen päivämäärä Fish Shell -ohjelmoinnin avulla? Yksinkertaisesti siksi, että ajanhallinta on tärkeä osa monia ohjelmointitehtäviä. Olipa kyseessä sitten tiedostojen tallentaminen ajastetusti tai komentojen toistaminen tiettynä päivänä, nykyisen päivämäärän saaminen helpottaa tehtävien suorittamista.

## Miten tehdä niin

Fish Shellillä on helppo saada nykyinen päivämäärä käyttämällä `date`-komentoa. Tämä komento palauttaa päivämäärän ja ajan kellonajalla varustettuna.

```
Fish Shell koodiesimerkki:

date

```
Tuloste:

```
tor elo 19 14:29:50 CEST 2021
```

Tämä vaihtoehto palauttaa kaikki tiedot päivämäärästä ja ajasta. Jos haluat vain päivämäärän ilman tarkempia yksityiskohtia, voit käyttää `+%d.%m.%Y`-merkkijonoa.

```
Fish Shell koodiesimerkki:

date +%d.%m.%Y
```

Tuloste:

```
19.08.2021
```

Voit myös muokata tulostetta muiden vaihtoehtojen avulla, esimerkiksi `+%A` palauttaa nykyisen viikonpäivän.

## Syvempi sukellus

Fish Shellin `date`-komento perustuu Unix-järjestelmän `date`-komentoon, joten sen käyttö on samanlaista myös muissa käyttöjärjestelmissä. Voit löytää lisätietoja `date`-komentosta Unixin manuaalisivuilta, joten jos haluat muokata tulosteen ulkoasua tai käyttää muita vaihtoehtoja, suosittelemme tutustumaan näihin sivuihin.

## Katso myös

- [Fish Shellin viralliset dokumentaatiot](https://fishshell.com/docs/current/index.html)
- [Fish Shellin GitHub-sivu](https://github.com/fish-shell/fish-shell)