---
title:    "Javascript: Komentoriviparametrien lukeminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoidessa meidän täytyy antaa ohjelmalle tietoa ennen sen suorittamista. Tämä tieto voi vaihdella sen mukaan, miten ohjelmaa käytetään tai mitä sen halutaan tekevän. Tässä tapauksessa käytämme komentoriviparametreja. Komentoriviparametrit ovat tietoa, jota syötämme ohjelmalle sen suorittamisen yhteydessä. Tästä syystä on tärkeää tietää, miten lukea komentoriviparametreja, jotta ohjelmamme voi toimia halutulla tavalla.

## Kuinka

Jos haluat lukea komentoriviparametreja Javascript-koodissasi, voit käyttää ```process.argv``` -muuttujaa. Tämä muuttuja sisältää taulukon kaikista komentoriviparametreista, jotka on annettu ohjelman suorittamisen yhteydessä. Voit käyttää ```slice()``` -funktiota, jotta saat taulukosta haluamasi komentoriviparametrit. Tässä esimerkissä näytämme, miten voit lukea komentoriviparametreja ja tulostaa ne konsoliin:

```Javascript
const parametrit = process.argv.slice(2); // Ensimmäinen parametri on aina 'node', toinen on tiedoston nimi, joten leikkaamme ne pois ja otamme halutut parametrit alkaen indeksistä 2
console.log(parametrit);
```

Jos haluat suorittaa ohjelman ja antaa sille samalla komentoriviparametreja, voit tehdä sen kirjoittamalla parametrit komennon perään erottaen ne välilyönnillä:

```
node index.js hello world
```

Tämä tulostaisi konsoliin taulukon ```["hello", "world"]```.

## Syvempi sukellus

On tärkeää huomata, että komentoriviparametrit ovat aina merkkijonoja, joten sinun täytyy muuntaa ne tarvittaessa muiksi datatyypeiksi käyttämällä esimerkiksi ```parseInt()``` tai ```parseFloat()``` -funktioita. Lisäksi voit myös tarkistaa, onko käyttäjä antanut tarvittavan määrän parametreja ohjelman suorittamisen yhteydessä käyttämällä ```process.argv.length``` -ominaisuutta. Näin voit varmistaa, että ohjelmasi ei kaadu, vaikka käyttäjä unohtaisi antaa kaikki tarvittavat parametrit.

## Katso myös

- [Node.js process.argv documentation](https://nodejs.org/dist/latest-v14.x/docs/api/process.html#process_process_argv)
- [MDN Web Docs - Command-line arguments](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt)
- [YouTube video: How to read command line arguments in Node.js](https://www.youtube.com/watch?v=nQicN01kH7I)