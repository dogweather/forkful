---
date: 2024-01-20 17:51:00.387197-07:00
description: "How to: - N\xE4in teet sen: String-interpolointi ei ole uusi juttu,\
  \ mutta JavaScriptin ES6-versiossa (ECMAScript 2015) se sai helpomman muodon template\u2026"
lastmod: '2024-04-05T22:51:11.082618-06:00'
model: gpt-4-1106-preview
summary: "- N\xE4in teet sen: String-interpolointi ei ole uusi juttu, mutta JavaScriptin\
  \ ES6-versiossa (ECMAScript 2015) se sai helpomman muodon template literaleina.\
  \ Ennen ES6:a koodarit k\xE4yttiv\xE4t `+`-operaattoria tai `concat`-metodia, mik\xE4\
  \ teki koodista helposti sekavaa. Template literaleissa k\xE4ytet\xE4\xE4n takakenoviivoja\
  \ (\\`) ja `${}`-syntaksia, mik\xE4 selkeytt\xE4\xE4 ilmaisujen ja muuttujien sijoittamista\
  \ merkkijonojen sis\xE4\xE4n. Vaihtoehtona on my\xF6s kirjastoja, kuten Lodash,\
  \ joka tarjoaa template-funktion. Kuitenkin, nyky\xE4\xE4n suositaan nativiin JavaScriptin\
  \ tuomaa simplicity\xE4. Template literaleita voi my\xF6s k\xE4ytt\xE4\xE4 monirivisten\
  \ merkkijonojen esitt\xE4miseen ilman, ett\xE4 joutuu k\xE4ytt\xE4m\xE4\xE4n rivinvaihto-merkkej\xE4\
  \ (\\n)."
title: Merkkijonon interpolointi
weight: 8
---

## How to: - Näin teet sen:
```JavaScript
// Perinteinen yhdistely +
let tervehdys = "Hei";
let nimi = "Maija";
console.log(tervehdys + ", " + nimi + "!"); // Output: Hei, Maija!

// ES6 Template literals (template strings) ``
let ikä = 25;
console.log(`${tervehdys}, ${nimi}! Olet ${ikä}-vuotias.`); // Output: Hei, Maija! Olet 25-vuotias.
```

## Deep Dive - Syväsukellus:
String-interpolointi ei ole uusi juttu, mutta JavaScriptin ES6-versiossa (ECMAScript 2015) se sai helpomman muodon template literaleina. Ennen ES6:a koodarit käyttivät `+`-operaattoria tai `concat`-metodia, mikä teki koodista helposti sekavaa. Template literaleissa käytetään takakenoviivoja (\`) ja `${}`-syntaksia, mikä selkeyttää ilmaisujen ja muuttujien sijoittamista merkkijonojen sisään.

Vaihtoehtona on myös kirjastoja, kuten Lodash, joka tarjoaa template-funktion. Kuitenkin, nykyään suositaan nativiin JavaScriptin tuomaa simplicityä.

Template literaleita voi myös käyttää monirivisten merkkijonojen esittämiseen ilman, että joutuu käyttämään rivinvaihto-merkkejä (\n).

```JavaScript
let runo = `
Oi kuu taivaan,
sinä yöllinen vahti.
Kuiskaat tuulen mukana,
unelmieni matkaa tähti.`;
console.log(runo);
```

## See Also - Katso myös:
- MDN Web Docs String interpolation: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- Template literal polyfills for supporting older browsers
- ECMAScript 2015 specification: http://www.ecma-international.org/ecma-262/6.0/
