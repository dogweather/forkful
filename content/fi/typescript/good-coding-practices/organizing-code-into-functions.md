---
date: 2024-01-26 01:16:28.707947-07:00
description: "Koodin j\xE4rjest\xE4minen funktioiksi tarkoittaa koodisi pilkkomista\
  \ uudelleenk\xE4ytett\xE4viksi, modulaarisiksi lohkoiksi. T\xE4m\xE4 tehd\xE4\xE4\
  n DRY (Don't Repeat Yourself)\u2026"
lastmod: 2024-02-19 22:05:15.215502
model: gpt-4-0125-preview
summary: "Koodin j\xE4rjest\xE4minen funktioiksi tarkoittaa koodisi pilkkomista uudelleenk\xE4\
  ytett\xE4viksi, modulaarisiksi lohkoiksi. T\xE4m\xE4 tehd\xE4\xE4n DRY (Don't Repeat\
  \ Yourself)\u2026"
title: "Koodin j\xE4rjest\xE4minen funktioihin"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioiksi tarkoittaa koodisi pilkkomista uudelleenkäytettäviksi, modulaarisiksi lohkoiksi. Tämä tehdään DRY (Don't Repeat Yourself) periaatteen mukaisesti, mikä tekee koodista siistimpää, helpommalukuisempaa ja virheenkorjauksen tuulahduksen.

## Miten:
Kuvitellaan, että teet peruslaskinta. Sen sijaan, että kirjoittaisit lisäyslogiikan joka kerta kun tarvitset sitä, luo `add` funktio:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Esimerkkitulostus: 12
```

Nyt, oletetaan että tarvitsemme funktion kertolaskulle:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Esimerkkitulostus: 12
```
Huomaatko, miten keskitymme yhteen tehtävään per funktio? Se on koodin järjestämisen ydin.

## Syväsukellus
Historiallisesti, ohjelmointikielten kehittyessä, funktiot muodostuivat olennaiseksi osaksi koodin rakennetta, pohjautuen matemaattisiin funktioihin. Ne ovat olennainen osa proseduraalista ohjelmointia ja elävät edelleen objektiorientoituneessa ja funktionaalisessa ohjelmointiparadigmassa.

Vaihtoehdot? Voisit vain olla käyttämättä funktioita, mutta se on yksisuuntainen lippu Spagettikaupunkiin. Tai voisit mennä OOP (Objektiivinen Ohjelmointi) reittiä ja pakata toiminnallisuuden metodeihin - jotka ovat käytännössä funktioita, jotka kuuluvat objekteihin.

Toteutuksen kannalta TypeScript vaatii tyyppejä. Syöte- ja paluuarvojen tyypittäminen funktioissa ei ole vain hyvien tapojen mukaista; se on välttämättömyys siistille TypeScript-koodille. Lisäksi, TypeScriptillä saat käyttöösi käteviä ominaisuuksia kuten ylukuormitukset, geneeriset tyypit ja valinnaiset parametrit funktioidesi tehostamiseen.

## Katso Myös
Tutustu näihin resursseihin parantaaksesi funktiopeliäsi:

- [TypeScript Käsikirja – Funktiot](https://www.typescriptlang.org/docs/handbook/2/functions.html): Raamattusi TypeScript funktioille.
- [Puhdasta Koodia JavaScriptilla](https://github.com/ryanmcdermott/clean-code-javascript#functions): Sovella Puhdasta Koodia periaatteita JavaScript-funktioihisi.
- [Et Tiedä JS:stä – Kattavuus & Sulkeumat](https://github.com/getify/You-Dont-Know-JS): Ymmärrä, miten funktiot toimivat kattavuuden ja sulkeumien kanssa JavaScriptissa.
