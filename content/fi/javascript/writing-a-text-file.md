---
title:    "Javascript: Tekstitiedoston kirjoittaminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

Miksi: Kirjoittamalla tekstiä, voit tallentaa tärkeitä tietoja ja jakaa niitä muiden kanssa helposti.

Miksi joku haluaisi kirjoittaa teksti-tiedoston? On monia syitä, kuten tallentaa muistiinpanoja, luoda ohjelmakoodia, tai kirjoittaa blogikirjoituksia. Teksti-tiedostot ovat myös loistava tapa tallentaa tietoja, joita haluat säilyttää pitkällä aikavälillä.

Miten: Alla on esimerkki, miten luoda ja kirjoittaa tiedostoon tekstiä Javascriptilla.

```Javascript
// Luodaan uusi tiedosto nimeltä "tekstifile.txt"
var tiedosto = new File("tekstifile.txt");

// Kirjoitetaan sisältöä tiedostoon
tiedosto.write("Tämä on teksti-tiedosto, jonka olemme luoneet Javascriptilla.");

// Suljetaan tiedosto
tiedosto.close();

// Luetaan tiedoston sisältö
var sisalto = File.read("tekstifile.txt");

// Tulostetaan sisältö konsoliin
console.log(sisalto);
```

Tämä esimerkki luo tiedoston nimeltä "tekstifile.txt", kirjoittaa siihen halutun sisällön ja lopulta lukee ja tulostaa tiedoston sisällön konsoliin. Voit muokata esimerkkiä haluamallasi tavalla luodaksesi ja kirjoittaaksesi tiedostoon.

Syöte:

```
Tämä on teksti-tiedosto, jonka olemme luoneet Javascriptilla.
```

Deep Dive: Teksti-tiedostoihin liittyy monia muita toimintoja, kuten tiedostojen avaaminen, muokkaaminen ja poistaminen. Voit myös käyttää Node.js:ää teksti-tiedostojen käsittelyyn. Voit hakea lisätietoa näiden toimintojen käytöstä internetistä tai lukemalla Javascript-oppikirjoja.

```Javascript
// Avaamme olemassaolevan tiedoston nimeltä "muistiinpanot.txt"
var tiedosto = new File("muistiinpanot.txt");

// Luetaan tiedoston sisältö
var sisalto = File.read("muistiinpanot.txt");

// Muokataan sisältöä
sisalto = sisalto.replace("ohjeet", "reseptit");

// Kirjoitetaan muokattu sisältö takaisin tiedostoon
tiedosto.write(sisalto);

// Suljetaan tiedosto
tiedosto.close();

// Poistetaan tiedosto
File.delete("muistiinpanot.txt");
```

Tämä esimerkki avaa tiedoston nimeltä "muistiinpanot.txt", muokkaa sen sisältöä ja kirjoittaa muokatun sisällön takaisin tiedostoon. Lopuksi esimerkki poistaa tiedoston. Tämä on vain yksinkertainen esimerkki teksti-tiedostojen käytöstä ja toiminnasta.

Katso myös: Tästä löydät lisää tietoa Javascriptin teksti-tiedostojen käytöstä ja toiminnoista:

1. https://developer.mozilla.org/en-US/docs/Web/API/File_system
2. https://www.w3schools.com/js/js_file_system.asp
3. https://codeburst.io/writing-to-files-with-node-js-d92a5c8703f
4. https://www.lucidchart.com/techblog/2017/12/04/working-with-files-in-javascript/

Nyt olet valmis aloittamaan teksti-tiedostojen luomisen ja käsittelyn Javascriptilla. Onnea matkaan!