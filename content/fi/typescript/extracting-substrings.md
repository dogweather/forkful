---
title:                "TypeScript: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringin erottaminen on hyödyllinen taito, joka auttaa parantamaan koodisi luettavuutta ja tehokkuutta. Se on erityisen hyödyllinen silloin, kun haluat muokata tai käsitellä tiettyä osaa merkkijonosta erillään muusta.

## Miten

Jos haluat erottaa merkkijonosta tietyn osan, voit käyttää TypeScriptin `substring()`-metodia. Tämä metodi ottaa kaksi parametria: aloitusindeksin ja lopetuspisteen. Näiden parametrien avulla voit määrittää, minkä osan merkkijonosta haluat erottaa.

Esimerkiksi, jos meillä on merkkijono "Tämä on esimerkki", ja haluamme erottaa siitä sanan "esimerkki", voimme käyttää seuraavaa koodia:

```TypeScript
let merkkijono: string = "Tämä on esimerkki";
let osa: string = merkkijono.substring(9, 18);
console.log(osa);
```

Tämä tulostaa "esimerkki" konsoliin. Huomaa, että aloitusindeksi alkaa aina 0:sta ja lopetuspiste ei sisälly eristettyyn osaan.

Voit myös käyttää `substring()`-metodia, jos haluat erottaa osan merkkijonosta sen alusta tai lopusta. Jos haluat erottaa ensimmäisen sanan "Tämä", voit käyttää seuraavaa koodia:

```TypeScript
let sana: string = merkkijono.substring(0, 4);
console.log(sana);
```

Tämä tulostaa "Tämä" konsoliin.

## Syvällinen sukellus

Vaikka `substring()` on helppo ja kätevä tapa erottaa osa merkkijonosta, on hyvä tietää, että se luo aina uuden merkkijonon sen sijaan, että muokkaisi alkuperäistä merkkijonoa. Tämä voi aiheuttaa suorituskykyongelmia, jos kyseessä on suuri merkkijono.

Toinen tärkeä asia huomioitavaa on, että `substring()` käyttää aloitusindeksejä ja lopetuspistettä, kun taas `substr()`-metodi käyttää aloitusindeksiä ja pituutta. Tämä voi aiheuttaa hämmennystä, joten on tärkeää pitää nämä kaksi metodia erillään.

## Katso myös

- [Microsoftin virallinen dokumentaatio `substring()`-metodista](https://docs.microsoft.com/en-gb/scripting/javascript/reference/substring-method-string-javascript)
- [MDN:n selitys `substring()`-metodista](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)