---
title:                "Tietokoneohjelmoinnissa standardivirheen kirjoittaminen"
html_title:           "TypeScript: Tietokoneohjelmoinnissa standardivirheen kirjoittaminen"
simple_title:         "Tietokoneohjelmoinnissa standardivirheen kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kirjoittaminen standardivirheeseen on tapa tulostaa virheilmoituksia ja muita tärkeitä viestejä ohjelman suorituksen aikana. Tämä auttaa ohjelmoijia tunnistamaan ja korjaamaan virheitä, jolloin ohjelma toimii mahdollisimman sujuvasti.

## Miten:
Esimerkki TypeScript-koodilla:
```
console.error("Tämä on virheviesti!");
```
Tulostus:
```
"Tämä on virheviesti!"
```

## Syvällisempi katsaus:
Kirjoittaminen standardivirheeseen on osa virheen käsittelyä ja virheilmoitusten hallintaa. Tämä käytäntö on ollut käytössä jo pitkään ohjelmoinnissa ja on edelleen tärkeä osa moderneja ohjelmointikieliä. Toisin kuin konsolin tulostukset, standardivirheen tulostukset tulevat punaisella värillä, mikä auttaa korostamaan virheitä ohjelman suorituksen aikana. On myös olemassa vaihtoehtoisia tapoja käsitellä virheitä, kuten kirjoittaminen standardilokiin, mutta standardivirheeseen kirjoittaminen on yleisesti suositeltavaa.

## Katso myös:
Lisätietoja TypeScriptista ja virheiden käsittelystä löytyy alla olevista lähteistä:
- [TypeScriptin viralliselta verkkosivustolta](https://www.typescriptlang.org/)
- [W3Schoolsin TypeScript-opetusohjelma](https://www.w3schools.com/typescript/default.asp)
- [MDN:n opas virheiden käsittelyyn JavaScriptissä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Control_flow_and_error_handling)