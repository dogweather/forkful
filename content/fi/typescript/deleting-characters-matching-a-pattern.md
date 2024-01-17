---
title:                "Pinnan mukaiset merkkien poistaminen"
html_title:           "TypeScript: Pinnan mukaiset merkkien poistaminen"
simple_title:         "Pinnan mukaiset merkkien poistaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkinpoisto, joka vastaa määrättyä kuvioa, on tapa poistaa tietyt merkit tietystä merkistöstä tai tekstimuodosta. Tätä tehdään usein ohjelmoinnin yhteydessä esimerkiksi tietojen puhdistamiseksi tai tietyn halutun tuloksen saamiseksi.

## Miten:
Esimerkki TypeScript-koodin käytöstä:
```
TypeScript function removeChars(input: string, pattern: string): string {
  return input.replace(new RegExp(pattern, 'g'), ''); 
}

console.log(removeChars('Hei Maailma!', '[aA]')); 

// Tulostaa: "Hei Mil!" 
```

## Syvälle sukellus:
Merkinpoistoa vastaava menetelmä on ollut käytössä jo vuosien ajan ohjelmoinnin maailmassa. Muita vaihtoehtoja voivat olla esimerkiksi käyttää säännöllisiä lausekkeita tai luoda oma algoritmi poistamista varten. TypeScriptin regex-toiminnallisuus tarjoaa kuitenkin helpon ja tehokkaan tavan suorittaa merkinpoisto ohjelmassa. Tämä tapahtuu muodostamalla uusi RegExp-objekti, jossa määritellään haluttu kuvio ja käytetään sitten replace-metodia poistamiseen.

## Katso myös:
- [RegExp-objekti TypeScriptissä](https://www.typescriptlang.org/docs/handbook/regexp.html)
- [Säännölliset lausekkeet ohjelmoinnissa](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Kustomoitava merkinpoisto suurilla datamäärillä TypeScriptillä](https://blog.totaljs.com/blogs/tutorials/20190305-custom-character-remover-for-large-strings-in-typescript/)