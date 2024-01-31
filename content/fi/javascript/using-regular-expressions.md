---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulaarilausekkeet (regex) ovat kaavoja tekstissä haun ja manipuloinnin tehostamiseen. Ne säästävät aikaa ja rivejä koodia tekstin monimutkaisten kuvioitten käsittelemisessä.

## How to:
```Javascript
// Yksinkertainen regex esimerkki: sähköpostiosoitteen etsiminen
const emailPattern = /\S+@\S+\.\S+/;
const text = "Ota yhteyttä example@domain.com kautta.";
const emailMatch = text.match(emailPattern);
console.log(emailMatch[0]); // Tulostaa: example@domain.com

// Lippujen käyttö: i lipulla unohdetaan kirjainkoon merkitys
const caseInsensitivePattern = /hello/i;
console.log("Hello world!".match(caseInsensitivePattern)); // Tulostaa: ['Hello']

// Korvaa-toiminto: vaihda kaikki esiintymät
const replacePattern = /vanha/g;
console.log("Vanha auto, vanha talo".replace(replacePattern, 'uusi')); // Tulostaa: Uusi auto, uusi talo
```

## Deep Dive
Regulaarilausekkeet tulivat käyttöön 1950-luvun algoritmi-tutkimuksessa. Niiden vaihtoehtoja ovat merkkijonojen metodeita kuten `indexOf` ja `includes`, mutta regex on tehokkaampi monissa tilanteissa. JavaScriptin regex toteutus perustuu Perl-kielen syntaksiin ja sen RegExp-olio käsittelee lausekkeita.

## See Also
- [Mozilla Developer Network, RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex101: Regex-testaustyökalu](https://regex101.com/)
- [Eloquent JavaScript, Regexp-luku](https://eloquentjavascript.net/09_regexp.html)
