---
title:    "Javascript: Sovitetun kaavan mukaisten merkkien poistaminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Joskus Javascript-ohjelmointia tehdessä saattaa olla tarpeen poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi auttaa esimerkiksi tiedon käsittelyssä tai tekstianalyysissä. Seuraavassa kerromme, miten tämä voidaan tehdä kätevästi Javascriptissä.

## Miten tehdä

Käytämme Javascriptissä sisäänrakennettua `replace()`-funktiota, jonka avulla voimme korvata merkkejä haluamillamme tiedoilla. Alla on esimerkki, jossa haluamme poistaa kaikki välilyönnit annetusta lauseesta ja tulostaa sen uudelleen muokattuna.

```Javascript
let lause = "Tämä on esimerkki lause";
let uusiLause = lause.replace(/ /g, "");
console.log(uusiLause);
```

Tämä koodi tuottaisi seuraavan tulosteen:

`Tämäonesimerkkilause`

Meidän täytyy ensin luoda uusi muuttuja `uusiLause`, johon tallennetaan alkuperäisestä lauseesta poistetut välilyönnit `replace()`-funktion avulla. Huomaa myös, että käytämme niin sanottua "regular expression" tai kaavatunnistetta `/ /g`, joka tarkoittaa kaikkia välilyöntejä (`" "`).

Voit myös muokata tätä koodia poistamalla tai muuttamalla muuta merkkijonoa. Esimerkiksi voit korvata haluamasi merkkijonon tai kirjaimen toisella, tai poistaa kaikki numerot tekstimuuttujasta.

## Syvä sukellus

`replace()`-funktion käyttö voi olla hyödyllistä myös monimutkaisempien kaavojen avulla. Voit esimerkiksi rajata, mitkä merkit poistetaan tai korvataan halutuilla kaavatunnisteilla. Esimerkiksi seuraava koodi poistaisi kaikki merkit lauseesta, jotka eivät ole kirjaimia tai välilyöntejä:

```Javascript
let lause = "Tämä on esimerkki lause, jonka avulla poistamme välimerkkejä. Esim:!@#$%^&*()";
let uusiLause = lause.replace(/[^a-z ]/gi, "");
console.log(uusiLause);
```

Tämä vaihtaisi kaikki numerot, erikoismerkit ja jopa väliä symbolit tyhjiksi merkeiksi, jättäen vain alkuperäiset kirjaimet ja välilyönnit jäljelle. Tulostaisi siis:

`Tämä on esimerkki lause jonka avulla poistamme välimerkkejä Esim `

## Katso myös

- [MDN `replace()`-dokumentaatio](https://developer.mozilla.org/fi/docs/orphaned/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools Regular Expressions Tutorial](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Javascript String Methods Cheat Sheet](https://www.freecodecamp.org/news/regex cheatsheet-with-examples/)