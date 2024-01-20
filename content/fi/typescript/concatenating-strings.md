---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

---

## Mitä & Miksi?

Liittäminen merkkijonoja on prosessi, jossa yhdistetään kaksi tai useampia merkkijonoja yhdeksi. Ohjelmoijat tekevät tätä esittämään ja manipuloimaan sikamääräistä tekstiä tehokkaasti.

## Miten

Alla tässä on muutamia tapoja, joilla voit yhdistää merkkijonot TypeScriptissä:

```Typescript
// 1. Perinteinen tapa plus-operaattoria käyttämällä
let tervehdys = 'Hei' + ' ' + 'Suomi';
console.log(tervehdys); // Tulostaa: Hei Suomi

// 2. ECMAScript 2015 -template literals
let nimi = 'Suomi';
tervehdys = `Hei ${nimi}`;
console.log(tervehdys); // Tulostaa: Hei Suomi
```
## Sukellus Syvemmälle

Historiallisesti merkkijonojen liittämisen ajateltiin olevan hidas prosessi joissakin ohjelmointikielessä, mutta moderneissa ympäristöissä, kuten TypeScriptissä, se on erittäin tehokasta. Jos haluat yhdistää suuren merkkijonojen määrän, kannattaa harkita taulukon käyttämistä ja `.join()` -menetelmää.

```Typescript
let sanat = ['Hei', 'mahtava', 'Suomi'];
let lause = sanat.join(' ');
console.log(lause); // Tulostaa: Hei mahtava Suomi
```

Tämä on erityisen hyödyllistä, jos haluat koota merkkijonon dynaamisesti luotavista osista. Join-menetelmän etu on, että se on yleensä nopeampi kuin perinteinen plus-operaattori erityisesti suurissa merkkijonotaulukoissa.

## Katso myös

1. [MDN Web Docs - String concatenation](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/First_steps/Strings#string_concatenation)


---

Toivottavasti tämä artikkeli antoi sinulle hyvän yleiskuvan siitä, kuinka yhdistää merkkijonot TypeScriptissä. Älä epäröi kokeilla eri lähestymistapoja ja löytää se, joka sopii parhaiten projektiisi.