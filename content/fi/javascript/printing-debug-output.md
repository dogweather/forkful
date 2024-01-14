---
title:    "Javascript: Virheenkorjaustulosteen tulostus"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Jokaisella ohjelmointikielillä on mahdollista tulostaa debug-tekstiä, ja Javascript ei ole poikkeus. Debug-tekstin tulostaminen on hyödyllistä, kun haluat tarkastella ohjelmakoodin suorituksen vaiheita ja mahdollisesti löytää virheitä. 

## Kuinka

Debug-tekstin tulostaminen Javascriptissä on helppoa. Voit käyttää esimerkiksi `console.log()` -funktiota, joka tulostaa haluamasi tekstin konsoliin. Voit myös käyttää `console.error()` -funktiota, joka antaa virheilmoituksen konsoliin. Jos haluat tulostaa muuttujan arvon, voit käyttää `console.log("Muuttujan arvo: " + muuttuja);` -muotoa.

```Javascript
console.log("Hei maailma!"); // tulostaa "Hei maailma!" konsoliin
var num = 10;
console.log("Muuttujan arvo: " + num); // tulostaa "Muuttujan arvo: 10" konsoliin
console.error("Virhe!"); // antaa virheilmoituksen konsoliin
```

## Syvällisemmin

Kun ohjelmakoodisi kasvaa, virheiden löytäminen ja korjaaminen voi olla haastavaa. Tässä kohtaa debug-tekstin tulostaminen tulee tarpeen. Voit tulostaa tekstiä konsoliin eri kohdissa ohjelmakoodia, jolloin näet mitä tapahtuu kunkin rivin kohdalla. Näin voit havaita, missä vaiheessa jokin muuttuja muuttaa arvoaan tai missä kohtaa ohjelma kaatuu.

Debug-tekstin tulostaminen voi myös auttaa ymmärtämään paremmin ohjelman suoritusta ja löytämään mahdollisia tehostamispisteitä. Voit esimerkiksi tulostaa laskutoimitusten välissä aikamerkinnän ja verrata niitä eri koodipätkien välillä. Tällä tavalla voit huomata, mikä kohta koodissa vie eniten aikaa ja mahdollisesti optimoida sitä.

## Katso myös

- [Javascriptin `console` -objekti](https://developer.mozilla.org/fi/docs/Web/API/console)
- [Debug-tekstin tulostaminen Visual Studio Codessa](https://code.visualstudio.com/docs/nodejs/nodejs-debugging)
- [Debug-tekstin tulostaminen Reactin kanssa käyttäen React Developer Tools -lisäosaa](https://reactjs.org/blog/2016/09/15/new-react-developer-tools.html)