---
date: 2024-01-20 17:47:44.087795-07:00
description: "How to: - Kuinka tehd\xE4\xE4n: Merkkijonon pituuden selvitt\xE4minen\
  \ JavaScriptill\xE4 on helppoa ja suoraviivaista: k\xE4yt\xE4 `length`-ominaisuutta.\
  \ Historiallisesti\u2026"
lastmod: '2024-04-05T22:51:11.088166-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka tehd\xE4\xE4n: Merkkijonon pituuden selvitt\xE4minen JavaScriptill\xE4\
  \ on helppoa ja suoraviivaista: k\xE4yt\xE4 `length`-ominaisuutta. Historiallisesti\
  \ t\xE4m\xE4 ominaisuus on ollut osa JavaScripti\xE4 alusta asti, eli ECMAScript\
  \ 1 -versiosta l\xE4htien. Ennen ES6:ta, jos halusi luoda uuden merkkijonon pituuden\
  \ perusteella, ainoa tapa oli k\xE4ytt\xE4\xE4 silmukoita tai funktioita. ES6 toi\
  \ mukanaan `String.prototype.repeat`-metodin, joka helpottaa t\xE4t\xE4 teht\xE4\
  v\xE4\xE4. Unicode-merkkijonojen kanssa pit\xE4\xE4 olla tarkkana, koska JavaScript\
  \ k\xE4sittelee Unicode-merkkej\xE4 erikoisesti. Esimerkiksi jotkut emotikonit voivat\
  \ koostua useammasta Unicode-skaalan merkist\xE4, mik\xE4 tarkoittaa ett\xE4 `length`-ominaisuus\
  \ voi antaa h\xE4m\xE4\xE4v\xE4n arvon."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## How to: - Kuinka tehdään:
```javascript
let greeting = "Hei maailma!";
console.log(greeting.length); // Tulostaa 12

let emptyString = "";
console.log(emptyString.length); // Tulostaa 0

let emojiString = "😊";
console.log(emojiString.length); // Tulostaa 2, koska Unicode-emotikonit voivat olla monimerkkisiä
```

## Deep Dive - Syväsukellus:
Merkkijonon pituuden selvittäminen JavaScriptillä on helppoa ja suoraviivaista: käytä `length`-ominaisuutta. Historiallisesti tämä ominaisuus on ollut osa JavaScriptiä alusta asti, eli ECMAScript 1 -versiosta lähtien.

Ennen ES6:ta, jos halusi luoda uuden merkkijonon pituuden perusteella, ainoa tapa oli käyttää silmukoita tai funktioita. ES6 toi mukanaan `String.prototype.repeat`-metodin, joka helpottaa tätä tehtävää.

Unicode-merkkijonojen kanssa pitää olla tarkkana, koska JavaScript käsittelee Unicode-merkkejä erikoisesti. Esimerkiksi jotkut emotikonit voivat koostua useammasta Unicode-skaalan merkistä, mikä tarkoittaa että `length`-ominaisuus voi antaa hämäävän arvon.

## See Also - Katso Myös:
- MDN Web Docs - String.length: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- JavaScript Unicode pitfalls: [http://mathiasbynens.be/notes/javascript-unicode](http://mathiasbynens.be/notes/javascript-unicode)
- Understanding ES6: [https://leanpub.com/understandinges6/read](https://leanpub.com/understandinges6/read)
