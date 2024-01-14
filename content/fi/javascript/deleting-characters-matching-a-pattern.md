---
title:                "Javascript: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

JavaScript-ohjelmoijat voivat haluta poistaa merkkejä, jotka vastaavat tiettyä kaavaa esimerkiksi datan puhdistamiseksi tai ei-toivotun tekstin poistamiseksi.

## Miten

Kärsivällistä lukijaa, lähdetään tutkimaan, miten poistaa merkkejä, jotka vastaavat tiettyä kaavaa JavaScriptillä.

```JavaScript
// Esimerkki syötteestä
const data = ['JavaScript', 'Python', 'PHP', 'Java', 'C++'];

// Poistetaan kaikki merkit, jotka sisältävät kirjaimen "p"
const newData = data.filter(element => !element.includes('p'));

console.log(newData); // Output: ['Java', 'C++'];

```

## Syvällinen sukellus 

Poistamisen lisäksi voimme myös korvata vastaavat merkit muulla merkillä tai toisella merkkijonolla. Voimme myös tarkastella käytössä olevien kaavojen tehokkuutta ja harkita eri vaihtoehtoja sen parantamiseksi. On myös tärkeää tarkistaa, että poistetut merkit vastaavat todella haluttua kaavaa.

## Katso myös

- [String replace method in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)