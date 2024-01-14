---
title:                "Bash: Tulevan tai menneen päivän laskeminen"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Bash-ohjelmointikieli voi tuntua aluksi pelottavalta ja monimutkaiselta, mutta sen avulla on mahdollista suorittaa monia hyödyllisiä tehtäviä. Yksi näistä on päivämäärien laskeminen tulevaisuutta tai menneisyyttä varten. Tämä voi olla hyödyllistä esimerkiksi laskiessa eräpäivää tai suunnitellessa tulevia tapahtumia.

## Kuinka tehdä se

Päivämäärien laskeminen Bashissa onnistuu helposti käyttämällä `date` -komennolla. Tämä komento antaa mahdollisuuden käsitellä päivämääriä eri muodoissa ja laskea niitä haluttuun suuntaan. Käytännössä tätä varten tarvitsemme kolme asiaa: lähtöpäivämäärän, päivien määrän, ja halutun suunnan (tuleva vai menneisyyteen).

- Lähtöpäivämäärä määritellään muodossa `YYYY-MM-DD`.
- Päivien määrä ilmoitetaan lisäämällä tai vähentämällä kokonaislukuja `+` tai `-` -merkkien perään.
- Suunta määritellään viimeisenä parametrina `day` tai `days`.

```Bash
# Laske päivämäärä 10 päivää tulevaisuuteen
date -d "2021-10-25 + 10 days"
```

Tämän komennon tulosteena saamme uuden päivämäärän, joka tässä tapauksessa on `2021-11-04`.

## Syvemmälle aiheeseen

`date` -komennolla on paljon muitakin vaihtoehtoja, joilla voit muokata päivämäärien laskemista haluamallasi tavalla. Voit esimerkiksi käyttää eri aikavyöhykkeitä, näyttää viikonpäivän tai vuoden päivän ja paljon muuta. Suosittelemme tutustumaan tarkemmin `date` -komennon manuaalisivuihin (`man date`) saadaksesi kaiken irti tästä kätevästä työkalusta.

## Katso myös

- [Bashin viralliset dokumentaatiot](https://www.gnu.org/software/bash/manual/)
- [Date-komennon manuaalisivu](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Date-Manipulation)

Kiitos lukemisesta ja onnea Bash-ohjelmointisi kanssa!