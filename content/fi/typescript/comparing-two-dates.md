---
title:                "TypeScript: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kahden päivämäärän välillä?

Kahden päivämäärän vertailu on tärkeää monissa ohjelmointitilanteissa, kuten sovelluksissa, joissa tulee käsitellä erilaisia aikaperusteisia tietoja. Tämä voi auttaa esimerkiksi päivämäärän valinnassa kalenterisovelluksessa tai tarkistaessa, onko tietty tapahtuma jo tapahtunut. Seuraavaksi näytämme, miten vertailla kahden päivämäärän välistä ajanjaksoa TypeScriptillä.

## Miten

Käytämme *Date* -rajapinnasta löytyviä metodeja ja ominaisuuksia tarkastaaksemme, missä järjestyksessä päivämäärät ovat toistensa suhteen. Ensiksi, luomme kaksi *Date* -muuttujaa, jotka sisältävät halutut päivämäärät:

```TypeScript
const ensimmäinenPäivämäärä = new Date('2020-05-01');
const toinenPäivämäärä = new Date('2020-06-01');
```

Voimme käyttää *getTime* -metodia saadaksemme päivämäärät millisekunteina ja verrata niitä keskenään. Jos ensimmäinen päivämäärä tapahtuu ennen toista, palautetaan negatiivinen luku, jos ne ovat samat, palautetaan 0 ja jos ensimmäinen päivämäärä tapahtuu myöhemmin, palautetaan positiivinen luku.

```TypeScript
ensimmäinenPäivämäärä.getTime(); // 1588300800000
toinenPäivämäärä.getTime(); // 1590969600000
```

Voimme myös käyttää *getTime* -metodia ja vähentää ensimmäisestä päivämäärästä toinen päivämäärä, jolloin saamme millisekuntien tarkan ajanjakson niiden välillä.

```TypeScript
toinenPäivämäärä.getTime() - ensimmäinenPäivämäärä.getTime(); // 2678400000 (31 päivää)
```

## Syventymistä

On huomioitava, että millisekunnit eivät välttämättä ole tarkin tapa vertailla päivämääriä, sillä ne eivät huomioi eri aikavyöhykkeitä tai karkausvuosia. Tarkemman vertailun tekemiseksi, voimme käyttää *getFullYear*, *getMonth* ja *getDate* -metodeita tarkistaaksemme jokainen päivämäärän osa erikseen.

```TypeScript
if (toinenPäivämäärä.getFullYear() < ensimmäinenPäivämäärä.getFullYear()) {
    console.log('Toinen päivämäärä tapahtuu ennen ensimmäistä.');
} else if (toinenPäivämäärä.getFullYear() > ensimmäinenPäivämäärä.getFullYear()) {
    console.log('Toinen päivämäärä tapahtuu myöhemmin kuin ensimmäinen.');
} else {
    if (toinenPäivämäärä.getMonth() < ensimmäinenPäivämäärä.getMonth()) {
        console.log('Toinen päivämäärä tapahtuu ennen ensimmäistä.');
    } else if (toinenPäivämäärä.getMonth() > ensimmäinenPäivämäärä.getMonth()) {
        console.log('Toinen päivämäärä tapahtuu myöhemmin kuin ensimmäinen.');
    } else {
        if (toinenPäivämäärä.getDate() < ensimmäinenPäivämäärä.getDate()) {
            console.log('Toinen päivämäärä tapahtuu ennen ensimmäistä.');
        } else if (toinenPäiv