---
aliases:
- /fi/javascript/interpolating-a-string/
date: 2024-01-20 17:51:00.387197-07:00
description: "String-interpolointi tarkoittaa muuttujien ja ilmaisujen yhdist\xE4\
  mist\xE4 merkkijonoihin. Koodareiden arjessa t\xE4m\xE4 on k\xE4yt\xE4nn\xF6llist\xE4\
  , sill\xE4 se helpottaa\u2026"
lastmod: 2024-02-18 23:09:08.020880
model: gpt-4-1106-preview
summary: "String-interpolointi tarkoittaa muuttujien ja ilmaisujen yhdist\xE4mist\xE4\
  \ merkkijonoihin. Koodareiden arjessa t\xE4m\xE4 on k\xE4yt\xE4nn\xF6llist\xE4,\
  \ sill\xE4 se helpottaa\u2026"
title: Merkkijonon interpolointi
---

{{< edit_this_page >}}

## What & Why? - Mitä ja miksi?
String-interpolointi tarkoittaa muuttujien ja ilmaisujen yhdistämistä merkkijonoihin. Koodareiden arjessa tämä on käytännöllistä, sillä se helpottaa dynaamisten tekstien luomista ja tekee koodista luettavampaa.

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
