---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:31.624544-07:00
description: "CSV:n (pilkuilla erotetut arvot) k\xE4sittely JavaScriptill\xE4 tarkoittaa\
  \ CSV-tiedostojen j\xE4sent\xE4mist\xE4 tai luomista, jotta voidaan joko ottaa vastaan\u2026"
lastmod: '2024-03-13T22:44:56.972205-06:00'
model: gpt-4-0125-preview
summary: "CSV:n (pilkuilla erotetut arvot) k\xE4sittely JavaScriptill\xE4 tarkoittaa\
  \ CSV-tiedostojen j\xE4sent\xE4mist\xE4 tai luomista, jotta voidaan joko ottaa vastaan\
  \ taulukkomuotoista dataa ulkoisista l\xE4hteist\xE4 tai vied\xE4 dataa k\xE4ytett\xE4\
  v\xE4ksi muissa ohjelmissa."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Mikä ja miksi?
CSV:n (pilkuilla erotetut arvot) käsittely JavaScriptillä tarkoittaa CSV-tiedostojen jäsentämistä tai luomista, jotta voidaan joko ottaa vastaan taulukkomuotoista dataa ulkoisista lähteistä tai viedä dataa käytettäväksi muissa ohjelmissa. Ohjelmoijat tekevät näin, koska se mahdollistaa helpon ja kevyen datan vaihdon sovellusten, tietokantojen ja järjestelmien välillä, joissa monimutkaisemmat formaatit kuten JSON voivat olla liioittelua.

## Miten:
JavaScriptissä ei ole sisäänrakennettua CSV:n jäsentämisen tai merkkijonoksi muuttamisen toiminnallisuutta kuten JSON:ssa. Voit kuitenkin käsitellä CSV-dataa helposti joko käyttämällä raakaa JavaScriptiä yksinkertaisempiin tehtäviin tai hyödyntämällä tehokkaita kirjastoja, kuten `PapaParse`, monimutkaisemmissa skenaarioissa.

### Perusjäsentäminen raakalla JavaScriptillä
CSV-merkkijonon jäsentäminen taulukoksi objekteja:

```javascript
const csv = `nimi,ikä,kaupunki
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const rivit = csv.split("\n");
  const tulos = [];
  const otsikot = rivit[0].split(",");

  for (let i = 1; i < rivit.length; i++) {
    const obj = {};
    const nykyinenRivi = rivit[i].split(",");
    
    for (let j = 0; j < otsikot.length; j++) {
      obj[otsikot[j]] = nykyinenRivi[j];
    }
    tulos.push(obj);
  }
  
  return tulos;
}

console.log(parseCSV(csv));
```
Tuloste:

```
[
  { nimi: 'John', ikä: '23', kaupunki: 'New York' },
  { nimi: 'Jane', ikä: '28', kaupunki: 'Los Angeles' }
]
```

### Perusluonti CSV:ksi raakalla JavaScriptillä
Objektien taulukon muuntaminen CSV-merkkijonoksi:

```javascript
const data = [
  { nimi: 'John', ikä: 23, kaupunki: 'New York' },
  { nimi: 'Jane', ikä: 28, kaupunki: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(rivi => 
    Object.values(rivi).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

Tuloste:

```
John,23,New York
Jane,28,Los Angeles
```

### PapaParse'n käyttö monimutkaisissa CSV-tehtävissä
Monimutkaisissa skenaarioissa `PapaParse` on vankka kirjasto, joka soveltuu CSV-tiedostojen jäsentämiseen ja merkkijonoksi muuttamiseen, tarjoten vaihtoehtoja suoratoistolle, työntekijöille ja suurten tiedostojen käsittelyyn.

CSV-tiedoston tai -merkkijonon jäsentäminen PapaParsella:

```javascript
// Kun olet lisännyt PapaParse-projektiisi
const Papa = require('papaparse');
const csv = `nimi,ikä,kaupunki
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(tulokset) {
    console.log("Jäsennetty:", tulokset.data);
  }
});
```

Tuottaa:

```
Jäsennetty: [
  ["nimi", "ikä", "kaupunki"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

Merkkijonon muuntaminen CSV-merkkijonoksi PapaParsella:

```javascript
const data = [
  { nimi: 'John', ikä: 23, kaupunki: 'New York' },
  { nimi: 'Jane', ikä: 28, kaupunki: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Luo:

```
nimi,ikä,kaupunki
John,23,New York
Jane,28,Los Angeles
```

Nämä esimerkit havainnollistavat perus- ja edistyneen tason CSV-käsittelyä JavaScriptillä, mahdollistaen helpon datan vaihdon web-sovelluksissa ja niiden ulkopuolella.
