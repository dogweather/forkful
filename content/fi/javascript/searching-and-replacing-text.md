---
title:    "Javascript: Tekstin etsiminen ja korvaaminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Kun ohjelmoit Javascriptillä, sinun on usein tarve etsiä ja vaihtaa tekstiä koodisi sisällä. Tämä voi olla esimerkiksi tarpeen korjata virheitä tai päivittää käytettyjä muuttujia. Onneksi Javascript tarjoaa helpon tavan suorittaa etsiminen ja tekstien vaihtaminen ohjelmassa.

## Kuinka tehdä

Kaikki lähtee Javascriptin sisäänrakennetusta `replace()` -funktiosta. Tämä funktio ottaa vastaan kolme parametria: etsittävän tekstin, korvaavan tekstin ja flag -parametrin. Flag -parametri on valinnainen ja sallii esimerkiksi kirjainkoolla tarkastelun.

Seuraavassa esimerkissä oletamme, että haluamme korvata kaikki nimesi "Mikko" tekstillä "Marko":

```Javascript
let teksti = "Tervetuloa, Mikko!";
let uusiTeksti = teksti.replace("Mikko", "Marko");

console.log(uusiTeksti);
```

Tämä koodi tuottaa seuraavan tulosteen:

```Javascript
"Tervetuloa, Marko!"
```

## Syvällisempi tarkastelu

Javascriptin `replace()` -funktio ei automaattisesti korvaa kaikkia esiintymiä, vaan ainoastaan ensimmäisen löytämänsä. Voit kuitenkin käyttää regex -ilmaisuja ja flag -parametria korvauksen tekemiseen kaikille esiintymille. Esimerkiksi seuraava koodi korvaa kaikki tekstin "Mikko" esiintymät:

```Javascript
let teksti = "Tervetuloa, Mikko ja Heini!";
let uusiTeksti = teksti.replace(/Mikko/g, "Marko");

console.log(uusiTeksti);
```

Tämä koodi tuottaa seuraavan tulosteen:

```Javascript
"Tervetuloa, Marko ja Heini!"
```

Regex -ilmaisut tarjoavat myös mahdollisuuden etsiä ja korvata tekstiä monimutkaisemmilla tavoilla, kuten käyttämällä säännöllisiä lausekkeita ja tunnisteita.

## Katso myös

- [MDN web docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [JavaScript Tutorial: Regular Expressions](https://www.javascripttutorial.net/javascript-regular-expression/)