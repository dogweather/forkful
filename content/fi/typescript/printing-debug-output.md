---
title:                "TypeScript: Vianjäljitystulostuksen toteutus"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi: 

Yksi tärkeimmistä osista ohjelmoinnissa on varmistaa, että koodi toimii oikein ja on helppo ymmärtää. Tulostamalla virheviestejä ja muuta debug-tietoa voimme helpottaa koodimme testaamista ja selvittää mahdolliset ongelmat nopeasti.

## Miten:

Debug-tulostukseen voidaan käyttää TypeScriptin `console`-komentoja. Esimerkiksi, jos haluaisimme tulostaa muuttujan `nimi` arvon konsoliin, käyttäisimme seuraavaa koodia:

```TypeScript 
console.log("Nimeni on " + nimi); 
```

Tämä tulostaisi konsoliin tekstin "Nimeni on [nimi]". Voimme myös tulostaa useita muuttujia ja muita tietoja kerralla käyttämällä `console.log` -komennon sisällä olevia pilkkuja.

## Syvällinen tarkastelu:

Tulostamalla debug-tietoja voimme tarkastella erilaisia virheitä ja havaita mahdollisia ongelmia koodissamme. Esimerkiksi voimme käyttää `console.error`-komentoa näyttämään virheviestejä, jos tiettyä ehtoa ei täytetä. Voimme myös käyttää `console.table` -komennolla näyttämään taulukkomuotoista dataa konsolissa.

Lisäksi, voimme asettaa ehtoja `console.assert` -komennolla tarkistamaan, että tietty ehto on totta ja muutoin tulostamaan virheilmoituksen.

Voimme myös käyttää TypeScriptin debugger-ominaisuutta tarkasti tutkimaan koodimme toimintaa. Voimme asettaa breakpointteja koodiin ja tarkkailla muuttujien arvoja ja suoritusjärjestystä.

## Katso myös:

- [TypeScript console -dokumentaatio](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html)
- [Debuggaus TypeScriptissa](https://www.tutorialsteacher.com/typescript/debugging-typescript-code)
- [TypeScript debuggerin käyttö](https://code.visualstudio.com/docs/typescript/typescript-debugging)