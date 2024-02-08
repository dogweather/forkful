---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- fi/javascript/getting-the-current-date.md
date:                  2024-02-03T19:09:59.912331-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Nykyisen päivämäärän saaminen JavaScriptillä on perustehtävä, johon kuuluu tämän päivän päivämäärän ja ajan noutaminen ja mahdollisesti manipulointi. Ohjelmoijat suorittavat tämän näyttääkseen päivämääriä verkkosivuilla, sovelluksissa, seuratakseen käyttäjän vuorovaikutuksia tai käsitelläkseen aikaan sidottua dataa.

## Kuinka:
Vanilla JavaScriptissä `Date`-objektia käytetään päivämäärien ja aikojen käsittelyyn. Tässä on miten voit saada nykyisen päivämäärän ja ajan:

```javascript
const currentDate = new Date();
console.log(currentDate); // Esimerkkituloste: Pe huhti 14 2023 12:34:56 GMT+0100 (Britannian kesäaika)
```

Näyttääksesi pelkän päivämäärän käyttäjäystävällisemmässä muodossa, voit käyttää metodeja kuten `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Esimerkkituloste: 14.4.2023
```

Saat enemmän hallintaa muodon ylle, kolmannen osapuolen kirjastot kuten *Moment.js* tai *date-fns* ovat hyvin suosittuja, vaikka on hyvä olla tietoinen siitä, että Moment.js on nyt pidetty vanhentuneena projektina ylläpitotilassa.

Käyttäen *Moment.js*:

```javascript
const moment = require('moment'); // olettaen Node.js:n käytön tai moduulipaketin käytön
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Esimerkkituloste: 2023-04-14
```

*date-fns*:n kanssa, joka korostaa modularisaatiota sallien sinun tuoda vain tarvitsemasi:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Esimerkkituloste: 2023-04-14
```

Kukin lähestymistapa tarjoaa eri tasoja mukavuutta ja joustavuutta työskenneltäessä päivämäärien kanssa JavaScriptissä, sisäänrakennetusta `Date`-objektista monimutkaisempiin muotoilu- ja manipulointimahdollisuuksiin, jotka ovat saatavilla kirjastojen kautta.
