---
title:    "TypeScript: Satunnaisten lukujen luominen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi: Satunnaislukujen luominen

Satunnaislukujen luominen on olennainen osa monia ohjelmointiprojekteja. Ne voivat olla tarpeen testauksessa, tietokonepelien luomisessa tai jopa salausmenetelmissä. Satunnaislukujen avulla voimme luoda ennustamattomia tapahtumia ja loputtomia mahdollisuuksia, mikä tekee ohjelman toiminnoista jännittävämpiä ja monipuolisempia.

## Miten: Koodiesimerkkejä satunnaislukujen luomiseen TypeScriptissä

Koodiesimerkit esitetään TypeScript-koodilohkoissa, jotka alkavat merkkijonolla "```TypeScript", sisältävät kirjoitetun koodin ja päättyvät "```". Tämän avulla on helppo visualisoida ja ymmärtää satunnaislukujen luontiprosessia.

### Yksinkertainen satunnaisluku

Pieninin satunnaisluvun luomiseksi TypeScriptissä voimme käyttää `Math.random()` - metodia. Tämä metodi palauttaa desimaaliluvun välillä 0 ja 1.

```TypeScript
let satunnaisluku = Math.random();
console.log(satunnaisluku);
```

Esimerkkitulos:

```TypeScript
0.82146549832
```

Jos haluamme määrittää ylimmän rajoituksen luotavalle satunnaisluvulle, voimme käyttää `Math.floor()` -metodia. Esimerkiksi jos haluamme satunnaisluvun väliltä 0 ja 10, määritetään rajoitus arvoon 11, koska `Math.floor()` pyöristää alaspäin.

```TypeScript
let satunnaisluku = Math.floor(Math.random() * 11);
console.log(satunnaisluku);
```

Esimerkkitulos:

```TypeScript
7
```

### Satunnaisen numeron arvauspeli

Käytetään nyt satunnaislukua toteuttamaan yksinkertainen numeron arvauspeli. Luodaan ensin satunnaisluku väliltä 1 ja 10, ja käytetään `while`-silmukkaa arvausten vastaanottamiseen ohjelman käyttäjältä.

```TypeScript
let oikeaLuku = Math.floor(Math.random() * 10) + 1;
let arvaus = prompt("Arvaa numero väliltä 1 ja 10");
while(arvaus != oikeaLuku){
  if(arvaus > oikeaLuku){
    console.log("Liian suuri, yritä uudelleen!");
  } else {
    console.log("Liian pieni, yritä uudelleen!");
  }
  arvaus = prompt("Arvaa numero väliltä 1 ja 10");
}
console.log("Hienoa, arvasit oikean numeron!");
```

Esimerkkitulos:

```TypeScript
Arvaa numero väliltä 1 ja 10
5
Liian suuri, yritä uudelleen!
Arvaa numero väliltä 1 ja 10
3
Liian pieni, yritä uudelleen!
Arvaa numero väliltä 1 ja 10
4
Hienoa, arvasit oikean numeron!
```

## Syventävä sukellus: Tietoa satunnaislukujen generoinnista

Satunnaislukujen generointi tietokoneessa ei ole täysin sattumanvaraista. Se perustuu matemaattisiin algoritmeihin, jotka voidaan ennustaa ja toistaa. Tämä tarkoittaa sitä, että luotujen lukujen ei kerrota olevan täysin satunnaisia, vaan pseudo-satunnaisia.

On myös tärkeää ymmärtää, että satunnaislukujen generoiminen tietokoneessa vaatii aloitusarvon, jota k