---
title:                "Komentoriviparametrien lukeminen"
html_title:           "TypeScript: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Komentorivin argumenttien lukeminen on tapa, jolla ohjelman koodi lukee ja käsittelee syötteitä käyttäjältä. Tämä voi olla hyödyllistä esimerkiksi ohjelman asetusten asettamisessa tai tietokannan käyttäjän tarjoamien hakuehtojen käsittelyssä. Komentorivin argumenttien lukeminen on siis tapa, jolla ohjelmoijat voivat tehdä ohjelmistaan interaktiivisempia ja joustavampia.

## Miten:

```TypeScript
const args = process.argv.slice(2); // slice(2) poistaa ensimmäiset kaksi arvoa
console.log(args); // [ "argumentti1", "argumentti2", ... ]
```
Lähettämällä komennon esimerkiksi `node index.js argumentti1 argumentti2` konsolissa, käyttäjän antamat argumentit tallentuvat process.argv -taulukkoon. Tästä taulukosta voidaan sitten poistaa ensimmäiset kaksi arvoa ja loput argumentit käsitellä halutulla tavalla.

## Syvemmälle:

Komentorivin argumenttien lukeminen on ollut käytössä jo pitkään, ja sen tarpeellisuus riippuu ohjelman tarkoituksesta. Jos ohjelman ei tarvitse olla interaktiivinen, voidaan käyttää esimerkiksi ympäristömuuttujia tai erillistä konfiguraatiotiedostoa syötteiden asettamiseen.

## Katso myös:

[Tyypillisiä tapoja syötteiden antamiseen komentoriviltä Typescriptissä](https://codeburst.io/typical-ways-to-take-command-line-arguments-in-typescript-c228f7adb793)

[Typescript-blogi: Mitä uutta Typescript 4.2:ssa? (sisältää kohteen null-elnvyttäminen)](https://typescript-blogi.fi/2021/02/26/mita-uutta-typescript-4-2-ssa-sisaltaa-kohteen-null-etnvyttaminen/)