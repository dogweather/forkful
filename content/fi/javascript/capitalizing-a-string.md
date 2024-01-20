---
title:                "Merkkijonon pääkirjoitus"
html_title:           "Javascript: Merkkijonon pääkirjoitus"
simple_title:         "Merkkijonon pääkirjoitus"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonon isoiksi kirjoittaminen tarkoittaa jokaisen merkkijonon sisältämän sanan ensimmäisen kirjaimen muuttamista isoiksi kirjaimiksi. Ohjelmoijat tekevät tämän tehdäkseen tekstistä helpommin luettavan tai korostaakseen tiettyjen sanojen merkitystä.

## Kuinka näin:
Yksinkertainen esimerkki merkkijonon alkukirjainten muuttamisesta isoiksi Javascriptissä.

```Javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

console.log(capitalizeFirstLetter('javascript on upeaa!'));
```
Tämän palaute olisi seuraava:

```Javascript
'Javascript on upeaa!'
```
## Syvempi sukellus:
Ison kirjaimen käyttö otsikoinnissa on peräisin kirjapainotavasta, jossa tärkeiden sanojen korostaminen tehtiin painamalla niiden alkukirjain isommalla fontilla. Javascriptissä kuin muissakin ohjelmointikielissä, merkkijonon kapitalisointiin on eri tapoja. Esimerkiksi voit käyttää `toUpperCase()` metodia koko merkkijonon muuttamiseen isoihin kirjaimiin.

```Javascript
function capitalizeAllLetters(string) {
    return string.toUpperCase();
}

console.log(capitalizeAllLetters('Javascript on upeaa!'));
```

Tämän palaute olisi seuraava:

```Javascript
'JAVASCRIPT ON UPEAA!'
```
## Katso myös:
Lisätietoja merkkijonojen muokkaamisesta Javascriptin avulla löytyy seuraavista lähteistä:
1. [Mozilla Developer Network - String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
2. [Mozilla Developer Network - String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
3. [W3Schools - Javascript String Methods](https://www.w3schools.com/jsref/jsref_obj_string.asp)