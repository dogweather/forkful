---
title:                "Koodin järjestäminen funktioihin"
date:                  2024-01-26T01:16:28.707947-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin järjestäminen funktioihin"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/organizing-code-into-functions.md"
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
