---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Javascript: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Ketjutetun merkkijonon pituuden löytäminen on yksinkertaisesti kyky tietää, kuinka monta merkkiä sisältää ketju. Tämä on hyödyllinen tehtävä ohjelmoinnissa, koska se auttaa löytämäänmikä tahansa merkkijono on, kuinka monta merkkiä se sisältää ja kuinka hyvin se soveltuu eri tarkoituksiin.

## Kuinka:
```Javascript 
let string = 'Tämä on esimerkki stringistä';
console.log(string.length); 
```

Tässä esimerkissä olemme määrittäneet muuttujan nimeltä string ja asettaneet sille arvon, joka on merkkijono "Tämä on esimerkki stringistä". Tämän jälkeen käytämme `.length` metodia saadaksemme ketjun pituuden ja tulostamme sen konsoliin. Tulostettu tulos on 27, koska sana "stringistä" sisältää 9 merkkiä ja välilyönnit lasketaan myös mukaan.

## Syvällä:
Stringin pituuden löytäminen on ollut käytössä ohjelmoinnissa jo vuosien ajan. Aikaisemmin tätä tehtävää varten oli käytettävä monia eri tapoja, kuten for-looppeja tai substringeja. Nykyisin `.length` metodi on helpottanut tätä tehtävää huomattavasti. On myös huomionarvoista, että tämä metodi toimii myös muilla javascriptin tietotyypeillä, kuten numerolla tai taulukoilla.

## Katso myös:
- [MDN dokumentaatio stringin pituudesta](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [JavaScript 30 -kurssi: stringin pituuden löytäminen](https://javascript30.com/)
- [Stringin käyttäminen ja manipulointi Javascriptissä](https://www.freecodecamp.org/news/javascript-string-manipulation-2a1a0ea7fca0/)