---
title:    "Gleam: Tiedoston lukeminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Miksi lukea tiedostot tekstipohjaisessa ohjelmointikielessä?

Monet ohjelmoijat ovat tottuneet käyttämään graafisia käyttöliittymiä, mutta tekstipohjaiset ohjelmointikielet, kuten Gleam, voivat tarjota tehokkaan ja nopean tavan lukea ja käsitellä tiedostoja. Tämä voi olla hyödyllistä esimerkiksi tiedon jalostamisessa tai suurten tietomäärien käsittelyssä.

## Näin tehdään: Lukeminen tiedostosta Gleamilla

Tekstipohjaiset ohjelmointikielet käyttävät usein samanlaista syntaksia tiedostojen lukemiseen. Gleamissa tämä tapahtuu ```File``` kirjaston avulla. Esimerkiksi jos haluamme lukea tekstikentän sisällön txt-tiedostosta nimeltä "tiedosto.txt", voimme käyttää seuraavaa koodia:

```Gleam
let tiedoston_sisalto = File.read("tiedosto.txt")
```

Tämän jälkeen voimme käsitellä tiedoston sisältöä haluamallamme tavalla. Voimme esimerkiksi tulostaa sen konsoliin:

```Gleam
let tiedoston_sisalto = File.read("tiedosto.txt")
Console.log("Tiedoston sisältö:", tiedoston_sisalto)
```

Tämän koodin ajamisen jälkeen näemme konsolissa tekstikentän sisällön. Huomaa, että voimme myös käyttää ```File``` kirjaston erilaisia funktioita, kuten ```File.exists()``` tarkistaaksemme, onko tiedosto olemassa ennen sen lukemista.

## Syvemmälle: Tiedostojen lukeminen Gleamilla

Gleamilla on monia erilaisia tapoja lukea tiedostoja, jotka tarjoavat lisää joustavuutta ja vaihtoehtoja erilaisten tiedostojen lukemiseen. Näihin kuuluvat esimerkiksi ```File.read_to_string()```, joka palauttaa tiedoston sisällön merkkijonona, ja ```File.read_to_data()```, joka palauttaa tiedoston sisällön binäärimuodossa. Voit myös tarkistaa Gleam-oppaasta tai dokumentaatiosta lisätietoja eri kirjastoista ja niiden käytöstä tiedostojen lukemiseen.

# Katso myös

- Gleam dokumentaatio: https://gleam.run
- Gleam-opas: https://gleam.run/book/getting-started.html
- File-kirjasto: https://gleam.run/modules/gleam_io/file.html