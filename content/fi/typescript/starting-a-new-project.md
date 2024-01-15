---
title:                "Uuden projektin aloittaminen"
html_title:           "TypeScript: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Uuden projektin aloittaminen voi tuntua haastavalta ja pelottavalta, mutta se voi myös tarjota lukuisia etuja. Pääset puhtaalta pöydältä luomaan jotain uutta ja voit hyödyntää uusimpia teknologioita ja menetelmiä.

## Miten aloittaa uusi projekti
Jos olet kiinnostunut aloittamaan uuden projektin TypeScriptillä, tässä on muutama yksinkertainen askel johdantona.

```TypeScript
// Luodaan muuttuja merkkijonon tyyppisellä arvolla
let nimi: string = "Matti";

// Tämä funktio tervehtii käyttäjää ja yhdistää nimen muuttujasta
function tervehdi(nimi: string) {
  console.log("Hei " + nimi + ", tervetuloa TypeScript-maailmaan!");
}
// Kutsutaan funktiota ja annetaan sille muuttujan arvo
tervehdi(nimi);

/* Tulostaa:

Hei Matti, tervetuloa TypeScript-maailmaan!
*/
```
Kuten näet, TypeScriptissä on mahdollista määritellä muuttujien tyyppi ja siten vähentää mahdollisia virheitä koodissa. Voit myös hyödyntää TypeScriptin tarjoamia muita ominaisuuksia, kuten luokkia ja rajapintoja, projektissasi.

## Syvällinen sukellus
Jotta aloitat uuden projektin TypeScriptillä, sinun tulee ensin asentaa TypeScriptin kehitysympäristö, kuten Visual Studio Code tai WebStorm. Tämän jälkeen voit luoda uuden TypeScript-projektin ja konfiguroida sen tarpeen mukaan.

On myös tärkeää muistaa pitää TypeScript-koodi järjestettynä ja hyvin dokumentoituna, jotta muut tiimin jäsenet voivat helposti ymmärtää ja muokata sitä.

## Katso myös
- [TypeScriptin virallinen sivusto](https://www.typescriptlang.org/)
- [TypeScript-opetusohjelma](https://www.typescriptlang.org/docs/handbook/typescript-from-scratch.html)
- [TypeScriptin aloittelijan opas](https://github.com/xcatliu/typescript-tutorial)