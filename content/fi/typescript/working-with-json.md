---
title:                "Työskentely jsonin kanssa"
html_title:           "TypeScript: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

JSON eli JavaScript Object Notation on tapa tallentaa ja välittää tietoa JavaScript-objekteina. Se on yleisesti käytetty muoto tietojen siirtoon sovellusten välillä. JSON on helposti luettava ja ymmärrettävä, mikä tekee siitä suositun valinnan ohjelmoijille.

## Kuinka:

```TypeScript
let data = {
    name: "John",
    age: 30,
    hobbies: ["reading", "coding", "music"],
}

// Muuntaa JavaScript-objektin JSON-merkkijonoksi
let json = JSON.stringify(data);

// Tulostaa JSON-merkkijonon
console.log(json);

// Muuntaa JSON-merkkijonon JavaScript-objektiksi
let obj = JSON.parse(json);

// Tulostaa JavaScript-objektin
console.log(obj);
```

Tulos:

```TypeScript
{"name":"John","age":30,"hobbies":["reading","coding","music"]}
{name: "John", age: 30, hobbies: ["reading", "coding", "music"]}
```

## Syväsukellus:

JSON kehitettiin vuonna 2001 Douglas Crockfordin toimesta, joka halusi standardoida tavan siirtää tietoa JavaScript-sovellusten välillä. Sittemmin JSON on noussut yhdeksi suosituimmista tiedonsiirtomuodoista.

Muita vaihtoehtoja JSONille ovat esimerkiksi XML ja CSV. Vaikka XML on ollut käytössä pidempään, JSON on yleisesti ottaen nopeampi ja helpommin luettava. CSV on yksinkertaisempi muoto, joka sopii paremmin numeeristen tietojen välittämiseen.

JSONin toteutus TypeScriptissa on mahdollista käyttämällä JSON-rajapintaa, joka tarjoaa metodeja JSON-tietojen muuntamiseen JavaScript-objekteiksi ja päinvastoin.

## Katso myös:

- [MDN web docs: JSON](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [TypeScript Doku: JSON](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html#new-json-file-handling)
- [JSON.org](https://www.json.org/)