---
title:                "Javascript: Merkkijonon muuttaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi
Miksi muuttaa merkkijono pieniksi kirjaimiksi? On olemassa monia syitä, miksikö ohjelmointikielen käyttäjän kannattaa muuntaa merkkijono pieniksi kirjaimiksi. Kun merkkijono on muutettu pieniksi kirjaimiksi, se on helpompi vertailla muihin merkkijonoihin ja suorittaa hakuja. Esimerkiksi käyttäjätunnusten ja salasanojen tapauksessa, pienet kirjaimet auttavat välttämään kirjoitusvirheitä ja lisäävät tietoturvaa.

## Ohjeet
Koska Javascriptin sisäänrakennettuja metodeja on helppo käyttää, on merkkijonon muuttaminen pieniksi kirjaimiksi yksinkertaista. Tässä on yksi tapa tehdä se:

```Javascript
let merkkijono = "TÄMÄ ON TESTI";
console.log(merkkijono.toLowerCase());
```

Tämän koodin tulosteksi saadaan `tämä on testi`. Näin yksinkertaisesti merkkijonon muuntaminen pieniksi kirjaimiksi Javascriptissa tapahtuu.

## Syvällinen tarkastelu
Javascript tarjoaa sisäänrakennettuja metodeja, kuten `toLowerCase()` ja `toUpperCase()`, jotka mahdollistavat merkkijonon muuntamisen halutun kokoisiksi kirjaimiksi. Nämä metodit ovat erityisen käteviä, kun tarvitsemme tietoja tietokannan haussa tai vertailussa, sillä ne helpottavat kirjoitusvirheiden välttämistä.

On myös hyvä huomata, että merkkijonon muuntamisella pieniksi tai suuriksi kirjaimiksi on merkitystä kielitieteen kannalta. Esimerkiksi suomenkielelle on olemassa sekä suur- että pienikirjaimiset verbimuodot, jotka eroavat toisistaan aikamuodon ja persoonan mukaan. Tämä tarkoittaa sitä, että merkkijonon muuntaminen pieniksi kirjaimiksi saattaa vaikuttaa myös sen merkitykseen ja voi aiheuttaa ongelmia tietyssä kontekstissa.

## Katso myös
- [Javascript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [Javascript Coding Basics: Manipulating Strings](https://blog.udemy.com/code-string-in-javascript/)
- [Wikipedia: Merkkijono](https://fi.wikipedia.org/wiki/Merkkijono)