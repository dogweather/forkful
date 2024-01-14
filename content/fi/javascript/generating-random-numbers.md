---
title:    "Javascript: Satunnaislukujen luominen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi 

Miksi ohjelmoija haluaisi luoda satunnaisia numeroita? Satunnaislukugeneraattorit ovat hyödyllisiä muun muassa pelien kehittämisessä, simulaatioissa ja tilastollisessa analyysissä. Ne voivat myös lisätä monimutkaisuutta ja haastetta ohjelmointitehtäviin sekä auttaa ratkaisemaan ongelmia, jotka vaativat satunnaisuuden elementin.

## Miten 

Random-luokka ohjelmointikielessä Javascript tarjoaa helpon ja kätevän tavan luoda satunnaisia numeroita. Alla on esimerkkejä eri tavoin käyttää tätä luokkaa ja tulosteita, jotka ne tuottavat:

```Javascript
// Luodaan satunnainen kokonaisluku 1 ja 10 välillä
let luku = Math.floor(Math.random() * 10) + 1;
console.log(luku); // Tulostaa esimerkiksi 7

// Luodaan satunnainen liukuluku 0 ja 1 välillä
let luku = Math.random();
console.log(luku); // Tulostaa esimerkiksi 0.489623495

// Luodaan satunnainen kokonaisluku tietystä väliltä
function satunnainen(vahintaan, enintään) {
  return Math.floor(Math.random() * (enintään - vahintaan + 1)) + vahintaan;
}

let luku = satunnainen(5, 15);
console.log(luku); // Tulostaa esimerkiksi 12
```

## Syväsukellus 

Satunnaislukujen luominen ei aina tuota täysin satunnaisia tuloksia. Monet algoritmit perustuvat seed-arvoihin, jotka vaikuttavat lopulliseen tulokseen. Lisäksi tietokoneet eivät todella pysty tuottamaan aidosti satunnaisia lukuja, vaan ne hyödyntävät matemaattisia kaavoja pohjana.

On myös tärkeää olla varovainen satunnaislukujen luomisessa esimerkiksi tietoturvallisuuden kannalta. On olemassa tehokkaita tapoja arpoa satunnaisia lukuja, jotka ovat vaikeasti arvattavissa ulkopuolisille.

## Katso myös 

- [MDN dokumentaatio satunnaislukujen luomisesta Javascriptillä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Artikkeli satunnaislukujen luomisesta tietoturvan kannalta](https://www.owasp.org/index.php/Testing_for_Weak_randomness_(OTG-CONFIG-006))
- [Blogipostaus eri tavoista käyttää satunnaislukugeneraattoria ohjelmoinnissa](https://www.toptal.com/developers/blog/understanding-randomness/)