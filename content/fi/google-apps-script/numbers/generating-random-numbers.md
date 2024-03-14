---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:07.531480-07:00
description: "Satunnaislukujen tuottaminen on ohjelmoinnissa perusteht\xE4v\xE4, jota\
  \ k\xE4ytet\xE4\xE4n lukuisissa sovelluksissa, kuten simulaatioissa, peleiss\xE4\
  \ ja\u2026"
lastmod: '2024-03-13T22:44:56.091879-06:00'
model: gpt-4-0125-preview
summary: "Satunnaislukujen tuottaminen on ohjelmoinnissa perusteht\xE4v\xE4, jota\
  \ k\xE4ytet\xE4\xE4n lukuisissa sovelluksissa, kuten simulaatioissa, peleiss\xE4\
  \ ja\u2026"
title: Sattumanvaraisten numeroiden generointi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen tuottaminen on ohjelmoinnissa perustehtävä, jota käytetään lukuisissa sovelluksissa, kuten simulaatioissa, peleissä ja turvajärjestelmissä. Ohjelmoijat käyttävät tätä tekniikkaa Google Apps Scriptissä tuodakseen vaihtelevuutta, testatakseen skenaarioita ja lisätäkseen ennustamattomuutta sovelluksiinsa Google-ekosysteemissä, mukaan lukien Sheets, Docs ja Forms.

## Kuinka:

Google Apps Scriptissä voit tuottaa satunnaislukuja käyttämällä `Math.random()`-funktiota, samoin kuin JavaScriptissä. Tämä funktio palauttaa liukuluvun, pseudo-satunnaisen luvun väliltä 0 (sisältyen) - 1 (ei sisältyen). Jotta nämä luvut voisi räätälöidä erilaisiin käyttötarkoituksiin, kuten kokonaislukujen tuottamiseksi tietyssä välissä, saatat tarvita lisälaskelmia.

### Perussatunnaisluvun tuottaminen

Jotta voit tuottaa yksinkertaisen satunnaisluvun ja kirjata sen konsoliin:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Esimerkkituloste:* `0.1234567890123456`

### Kokonaisluvun tuottaminen tietyssä välissä

Jotta voit tuottaa satunnaisen kokonaisluvun kahden arvon (`min` ja `max`) välillä, mukaan luettuna:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Esimerkki:
getRandomInt(1, 10);
```
*Esimerkkituloste*: `7`

Muista, että `Math.ceil()`-funktiota käytetään pienimmän arvon pyöristämiseen ylöspäin, ja `Math.floor()`-funktiota käytetään suurimman arvon pyöristämiseen alaspäin, varmistaen, että satunnaisluku on määritellyssä välissä.

## Syväsukellus

Mekanismi satunnaislukujen tuottamiseen Google Apps Scriptissä, ja itse asiassa useimmissa ohjelmointikielissä, hyödyntää pseudo-satunnaislukugeneraattoria (PRNG). Tämä tekniikka on deterministinen ja nojaa alkuperäisarvoon, jota kutsutaan siemeneksi, tuottaakseen lukujonoa, joka vaikuttaa satunnaiselta. Vaikka se riittää monille sovelluksille, on tärkeää huomata, että pseudo-satunnaisluvut eivät välttämättä sovi kohteisiin, joissa vaaditaan korkeaa turvallisuutta tai todellista satunnaisuutta, kuten kryptografisissa sovelluksissa.

Todellista satunnaisuutta voidaan saavuttaa laitteistopohjaisilla satunnaislukugeneraattoreilla tai palveluilla, jotka tuottavat satunnaisuutta luonnollisista ilmiöistä. Kuitenkin useimmissa päivittäisissä käsikirjoitustarpeissa Google Apps Scriptissä, `Math.random()` riittää.

Historiallisesti pyrkimys tehokkaampien satunnaislukujen tuottamistekniikoiden kehittämiseen on johtanut erilaisten algoritmien kehittämiseen, joista merkittäviä esimerkkejä ovat Mersenne Twister ja Linear Congruential Generator (LCG). Kuitenkin, ottaen huomioon korkean abstraktion tason Google Apps Scriptissä, useimmat käyttäjät eivät tarvitse toteuttaa näitä algoritmeja suoraan, mutta periaatteiden ymmärtäminen voi auttaa arvostamaan satunnaislukujen tuottamisen tärkeyttä ja rajoituksia skripteissäsi.
