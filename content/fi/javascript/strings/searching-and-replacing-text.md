---
date: 2024-01-20 17:58:20.460227-07:00
description: "How to: (Kuinka tehd\xE4:) JavaScriptissa `String`-objektilla teht\xE4\
  v\xE4 etsi ja korvaa on suoraviivaista. Tutkitaan `replace()`-metodia."
lastmod: '2024-04-05T21:53:58.513721-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) JavaScriptissa `String`-objektilla teht\xE4v\xE4 etsi\
  \ ja korvaa on suoraviivaista."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## How to: (Kuinka tehdä:)
JavaScriptissa `String`-objektilla tehtävä etsi ja korvaa on suoraviivaista. Tutkitaan `replace()`-metodia:

```javascript
let teksti = "Hei Maailma! Maailma on suuri.";
let uusiTeksti = teksti.replace("Maailma", "Javaskripti");

console.log(uusiTeksti); // Tulostaa: Hei Javaskripti! Maailma on suuri.
```

Huomaat, että vain ensimmäinen "Maailma" korvautui. Globaalisti korvataksesi käytä regexiä:

```javascript
let globaaliTeksti = teksti.replace(/Maailma/g, "Javaskripti");

console.log(globaaliTeksti); // Tulostaa: Hei Javaskripti! Javaskripti on suuri.
```

## Deep Dive (Sukellus syvyyksiin)
Ennen moderneja JavaScript-versioita, tekstinkorjaus oli työläämpää ja hitaampaa. ECMAScript 2015 toi nuolifunktioita ja template-literal-syntaksia, mikä yksinkertaisti string-käsittelyä.

Vaihtoehtoisia menetelmiä ovat `split()` ja `join()` yhdistelmiä, jotka voivat tehdä saman asian:

```javascript
let vaihtoehtoinenTeksti = teksti.split("Maailma").join("Javaskripti");
console.log(vaihtoehtoinenTeksti); // Hei Javaskripti! Javaskripti on suuri.
```

Suorituskyvyn näkökulmasta `replace()`-metodi regexillä on yleensä tehokkain. Mutta jos haluat korvata jotain mutkikkaampaa, regex-syntaksi saattaa olla monimutkainen ja vaikeaselkoinen.

## See Also (Katso myös)
- MDN Web Docs - String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- RegExp-ohjeet: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
- JavaScript Info - Replace method: https://javascript.info/string#replacing-parts-of-a-string

Nykyaikaiset ohjelmointikielen ominaisuudet tekevät tekstinkäsittelystä näppärää ja nopeaa. Opettelemalla eri metodeja voit valita työkalun tehtävään sopivasti ja työstää stringejä tehokkaasti.
