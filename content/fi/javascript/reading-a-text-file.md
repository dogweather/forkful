---
title:                "Javascript: Tiedostotiedoston lukeminen"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tiedostojen lukeminen on olennainen osa ohjelmoinnin maailmaa, ja se voi olla tarpeen monissa tilanteissa. Lukemalla tiedostoja voit saada tarvittavia tietoja ja käsitellä niitä edelleen oman ohjelmasi sisällä. Tässä artikkelissa opit, miten voit lukea tekstiä sisältäviä tiedostoja käyttäen Javascriptiä.

## Miten

```Javascript
const fs = require('fs'); // tuo "fs" moduuli
const data = fs.readFileSync('tiedostonimi.txt', 'utf8'); // lataa ja lue tiedoston sisältö

console.log(data); // tulostaa tiedoston sisällön konsoliin
```

Tämä yksinkertainen esimerkki käyttää Node.js:n tarjoamaa "fs" moduulia, joka tarjoaa tarvittavat toiminnot tiedostojen lukemiseen. Ensimmäisellä rivillä tuomme em. moduulin ja tallennamme sen muuttujaan "fs". Seuraavaksi käytämme "fs"-muuttujaa "readFileSync" -funktion kautta, joka lukee tiedoston sisällön ja tallentaa sen "data"-muuttujaamme. Lopuksi tulostamme tiedoston sisällön konsoliin, jotta voimme nähdä sen.

```Javascript
const fs = require('fs'); // tuo "fs" moduuli
const data = fs.readFileSync('tiedostonimi.txt', 'utf8'); // lataa ja lue tiedoston sisältö

data.split('\n').forEach(function(line) { // pilkkoo tiedoston sisällön riveiksi ja loopataan läpi jokainen rivi
    console.log(line); // tulostaa yhden rivin kerrallaan
});
```

Tämä toinen esimerkki näyttää, miten voit käsitellä tiedoston sisältöä ja tehdä siitä enemmän kuin vain tulostaa sen konsoliin. Käytämme samaa koodia kuin ensimmäisessä esimerkissä, mutta tällä kertaa kutsutaan "split" -funktiota, joka jakaa tiedoston sisällön riviksi "\n" -merkin kohdalta. Tämän jälkeen käytämme "forEach" -funktiota loopataksemme läpi jokaisen rivin ja tulostamme sen konsoliin.

## Syvällinen sukellus

Tiedostojen lukeminen sisältää paljon muutakin kuin vain pelkän sisällön tulostamisen. Voit esimerkiksi käyttää "fs" moduulia myös tekemään muutoksia tiedostoon, kuten kirjoittamaan uutta sisältöä tai poistamaan vanhaa. Voit myös käyttää erilaisia funktioita, kuten "existsSync", joka antaa tiedon, onko haluamasi tiedosto olemassa vai ei. On myös huomioitava, että eri ohjelmointikielimillä on erilaisia tapoja lukea tiedostoja, joten kannattaa tutustua omaan kieliisi erilaisiin toimintoihin.

Markdown teksti, jota luet juuri nyt, on myös yksi tapa kirjoittaa tekstejä ja dokumentoida koodia. Kannattaa tutustua myös Markdowniin ja opetella sen käyttöä.

## Katso myös

- [Node.js "fs" moduulin dokumentaatio](https://nodejs.org/api/fs.html)
- [Markdown ohjeet](https://www.markdownguide.org/)