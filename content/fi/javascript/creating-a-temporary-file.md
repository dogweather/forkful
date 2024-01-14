---
title:    "Javascript: Väliaikaisen tiedoston luominen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Miksi luoda tilapäinen tiedosto?

Tilapäisten tiedostojen luominen on tärkeä osa monia ohjelmointiprojekteja, sillä ne tarjoavat väliaikaisen tallennustilan, jota voidaan käyttää datan väliaikaiseen tallentamiseen ja käsittelyyn. Tämä voi olla erityisen hyödyllistä, kun työskentelet isojen tiedostojen tai monimutkaisten tietorakenteiden kanssa.

## Näin luodaan tilapäinen tiedosto

Käyttämällä JavaScriptiä on mahdollista luoda tilapäisiä tiedostoja helposti ja vaivattomasti. Seuraavassa esimerkissä luomme tilapäisen tiedoston, joka sisältää joitain tekstejä ja tallennamme sen koneelle.

```Javascript
var tmp = require('tmp');
var fs = require('fs');

// Luodaan tilapäinen tiedosto
tmp.file(function(err, path, fd, cleanupCallback) {
    if (err) throw err;

    // Kirjoitetaan teksti tiedostoon
    fs.write(fd, 'Tämä on tilapäinen tiedosto', function(err) {
        if (err) throw err;

        // Konsoliin tulostetaan tiedoston luetut tiedot
        fs.readFile(path, 'utf8', function(err, data) {
            if (err) throw err;
            console.log(data);
        });

        // Poistetaan tiedosto
        cleanupCallback();
    });
});
```

Tässä esimerkissä käytämme Node-palvelinta luomaan tilapäisen tiedoston käyttämällä `tmp` -paketin `file` -metodia. Tämä metodi ottaa vastaan callback-funktion, joka suoritetaan kun tiedosto on luotu. Luodessamme tiedoston, annamme myös osoittimen tiedostoon (`fd`), jotta voimme kirjoittaa siihen.

Käyttämällä `fs` -pakettia, voimme kirjoittaa tiedostoon haluamamme tekstin käyttämällä `write` -metodia. Tämän jälkeen voimme lukea tiedoston sisällön käyttämällä `readFile` -metodia ja tulostaa sen konsoliin.

Lopuksi, käytämme `cleanupCallback` -funktiota poistaaksemme luodun tilapäisen tiedoston.

## Syvällinen sukellus

Tilapäisten tiedostojen luominen voidaan toteuttaa monilla eri tavoilla, mutta käyttämällä `tmp` -pakettia se on helppoa ja turvallista. Tämä paketti tarjoaa myös muita hyödyllisiä toimintoja, kuten tilapäisten hakemistojen luominen ja niiden poistaminen.

Kannattaa myös huomata, että tilapäisillä tiedostoilla on yleensä oletusarvoisesti "uniq" -nimi, mikä tarkoittaa sitä, että ne eivät korvaa olemassa olevia tiedostoja, vaan luovat aina uuden nimen.

## Katso myös

- [Tmp-paketin dokumentaatio](https://www.npmjs.com/package/tmp)
- [Node-palvelimen opetusohjelmat](https://nodejs.org/en/docs/guides/)