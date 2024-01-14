---
title:    "Javascript: Puominlukulauseiden lukeminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet kehittäjä ja haluat syventyä Javascript-ohjelmointiin, on tärkeää osata lukea komentorivin argumentteja. Tämä taito auttaa sinua luomaan monipuolisia ja joustavia ohjelmia.

## Kuinka

Jotta voit lukea komentorivin argumentteja Javascript-ohjelmassa, käytä process.argv-muuttujaa. Tämä muuttuja sisältää taulukon kaikista komentorivillä annetuista argumenteista.

```Javascript
// Esimerkki komentorivin argumenttien lukemisesta
const args = process.argv;

// Tulostetaan kaikki argumentit
console.log(args);

// Tulostetaan ensimmäinen argumentti
console.log(args[0]);

// Tulostetaan toinen argumentti
console.log(args[1]);
```

Komennolla "node tiedosto.js argumentti1 argumentti2" tulostuu seuraava taulukko:

```
[node, tiedosto.js, argumentti1, argumentti2]
```

## Syvällinen sukellus

Komentorivin argumenttien lukeminen voi olla hyödyllistä esimerkiksi silloin, kun ohjelman parametreja halutaan muuttaa käyttäjän tekemillä valinnoilla. Tämä voi olla hyödyllistä esimerkiksi käyttöliittymättömissä sovelluksissa tai komentoriviltä ajettavissa skripteissä.

On myös tärkeää huomata, että process.argv-muuttuja sisältää myös tiedoston sijainnin ensimmäisessä indeksissään, joten ensimmäinen argumentti sijaitsee indeksissä 2.

## Katso myös

- [Komentorivin argumenttien lukeminen Node.js-sovelluksessa (englanniksi)](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Node.js-dokumentaatio (englanniksi)](https://nodejs.org/docs/latest/api/)
- [Javascript-oppaat ja dokumentaatiot (suomeksi)](https://developer.mozilla.org/fi/docs/Web/JavaScript)