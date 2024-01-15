---
title:                "Tekstin pirstalointi"
html_title:           "TypeScript: Tekstin pirstalointi"
simple_title:         "Tekstin pirstalointi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Miksi

## Miksi joku haluaisi etsiä osamerkkijonoja?

Osamerkkijonojen etsiminen on hyödyllistä, kun halutaan tarkistaa, sisältääkö tietty merkkijono halutun osamerkkijonon tai kun halutaan manipuloida merkkijonoa ja poimia siitä tietyt osat.

## Miten

```TypeScript
// Alustetaan merkkijono
let sana: string = "tervetuloa";

// Haetaan osamerkkijono
let osa: string = sana.substring(0, 5);

// Tulostetaan tulos konsoliin
console.log(osa); // tulostaa "terve"
```

Merkkijonon `substring()`-metodi ottaa kaksi parametria: aloitusindeksin ja lopetusindeksin. Ohjelma palauttaa halutun osamerkkijonon aloittaen annetusta aloitusindeksistä ja päättyen ennen lopetusindeksiä. Huomaa, että indeksilaskenta alkaa aina nollasta. 

## Syvempi sukellus

Substringien etsiminen voi olla hyödyllistä myös silloin, kun halutaan suorittaa monimutkaisempia manipulaatioita merkkijonoilla. Esimerkiksi jos halutaan tarkistaa, sisältääkö syötetty luku tietyn lukumäärän numeroita, voidaan käyttää substringiä. Tämä onnistuu helposti esimerkiksi muuntamalla luku merkkijonoksi ja sitten tarkastamalla sen pituutta substring-metodilla.

Seuraavassa esimerkissä käydään läpi merkkijonon `slice()`-metodi, joka toimii samalla tavalla kuin `substring()`, mutta ottaa aloitus- ja lopetusindeksien sijaan aloitusindeksin ja halutun osan pituuden.

```TypeScript
// Alustetaan merkkijono
let sana: string = "tervetuloa";

// Haetaan osamerkkijono slice-metodilla
let osa: string = sana.slice(3, 7);

// Tulostetaan tulos konsoliin
console.log(osa); // tulostaa "vetu"
```

Huomaa, että kun käytetään negatiivisia lopetusindeksejä, merkkijonoa etsitään lopusta alkuun. Esimerkiksi jos käytämme `-3` lopetusindeksinä, tulee tulokseksi "loa".

## Katso myös

- [MDN Web Docs - substring()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - slice()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/slice)