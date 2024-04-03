---
date: 2024-01-26 01:40:55.619046-07:00
description: "Refaktorointi on olemassa olevan tietokonekoodin uudelleenrakentamisen\
  \ prosessi muuttamatta sen ulkoista k\xE4ytt\xE4ytymist\xE4. Ohjelmoijat tekev\xE4\
  t sit\xE4\u2026"
lastmod: '2024-03-13T22:44:56.958115-06:00'
model: gpt-4-0125-preview
summary: "Refaktorointi on olemassa olevan tietokonekoodin uudelleenrakentamisen prosessi\
  \ muuttamatta sen ulkoista k\xE4ytt\xE4ytymist\xE4."
title: "Koodin uudelleenj\xE4rjestely"
weight: 19
---

## Kuinka:
Katsotaan yksinkertaista esimerkkiä, jossa refaktorointi voi tehdä koodistasi tiiviimpää ja luettavampaa. Tässä me refaktoroimme funktion, joka laskee numeroiden taulukon summan.

Ennen:
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // Tuloste: 10
```

Jälkeen:
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // Tuloste: 10
```

Näetkö, kuinka `reduce`-metodi pienentää funktion kokoa samalla kun toiminnallisuus pysyy muuttumattomana? Se on refaktorointia sinulle.

## Syväluotaus
Refaktorointi ei noussut viralliseksi käytännöksi ennen kuin Martin Fowlerin kirjan "Refactoring: Improving the Design of Existing Code" julkaisua vuonna 1999. Tämä kirja yhdessä ketterän ohjelmistokehityksen nousun kanssa auttoi työntämään refaktoroinnin valtavirtaan.

Kuvata refaktorointi ohjelmistokehityksen näkökulmasta on kuin selittää, miksi siivoaisit työpajan: teet sen, jotta seuraavan kerran, kun sinun on korjattava jotain (tässä tapauksessa koodia), käytät vähemmän aikaa sotkun kanssa ja enemmän itse ongelman parissa.

Kun puhumme vaihtoehdoista refaktoroinnille, astumme laajempaan keskusteluun ohjelmiston ylläpitomenetelmistä. Voisi valita täydellisen uudelleenkirjoituksen, esimerkiksi, mutta se on usein kalliimpaa ja riskialttiimpaa. Refaktoroi asteittain, ja saat jatkuvia hyötyjä ilman, että laiva uppoaa äkillisen peruskorjauksen seurauksena.

Refaktorointia on auttanut integroitujen kehitysympäristöjen (IDEs) ja työkalujen, kuten JSHint, ESLint ja Prettier JavaScript-ekosysteemissä, kehitys, jotka automatisoivat koodinlaadun tarkistuksia ja korostavat refaktoroinnin mahdollisuuksia.

Kaikki on kiinni puhtaasta, ilmaisuvoimaisesta ja ylläpidettävästä koodista. Monimutkaiset algoritmit, tietorakenteiden optimoinnit tai jopa arkkitehtoniset muutokset, kuten siirtyminen proseduraalisesta funktionaaliseen ohjelmointityyliin, saattavat olla osa refaktorointiprosessia.

Refaktorointi on tehtävä varoen; on olennaista, että sinulla on vankka joukko testejä varmistamassa, että muutoksesi eivät ole muuttaneet ohjelmiston käyttäytymistä odottamattomasti - toinen syy, miksi testivetoinen kehitys (TDD) sopii hyvin yhteen refaktoroinnin kanssa, koska se tarjoaa tuon turvaverkon oletusarvoisesti.

## Katso Myös
- Martin Fowlerin Refaktorointi-kirja: [Refactoring - Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- JavaScript-testing Frameworkit (varmistamaan, ettei refaktorointi riko toiminnallisuutta):
  - Jest: [Jest - Delightful JavaScript Testing](https://jestjs.io/)
  - Mocha: [Mocha - hauska, yksinkertainen, joustava JavaScript-testikehys](https://mochajs.org/)

- Työkalut Koodinlaadun ja Refaktoroinnin Tukeen:
  - ESLint: [ESLint - Pluggable JavaScript Linter](https://eslint.org/)
  - Prettier: [Prettier - Mielipidekoodin Muotoilija](https://prettier.io/)
