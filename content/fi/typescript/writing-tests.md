---
title:                "Testien kirjoittaminen"
html_title:           "TypeScript: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Testien kirjoittaminen on tärkeä osa ohjelmoinnin prosessia, joka auttaa varmistamaan koodin toimivuuden ja vähentämään virheiden määrää. Se myös auttaa tekemään muutoksia ja lisäyksiä helpommin ja nopeammin.

## Miten
Testien kirjoittaminen TypeScriptillä on helppoa ja intuitiivista. Voit aloittaa luomalla uuden tiedoston *.test.ts ja kirjoittamalla testejä "```TypeScript ... ```" -lohkoihin. Esimerkiksi:

```TypeScript
// Luodaan testifunktio
function add(x: number, y: number): number {
    return x + y;
}

// Testataan funktiota
describe("add-funktio", () => {

    test("palauttaa oikean tuloksen", () =>{
        expect(add(2, 3)).toEqual(5); // Odotetaan tuloksen olevan 5
    });

    test("palauttaa numeron", () =>{
        expect(typeof add(2,3)).toEqual("number"); // Odotetaan numeron olevan paluuarvo
    });
});
```

Yllä olevassa esimerkissä luomme yksinkertaisen funktion, joka lisää kaksi numeroa yhteen. Sen jälkeen testaamme tätä toimintoa käyttäen describe- ja test-lohkoja. Describe sisältää testiryhmän nimen ja testit sisältävät koodin, jota haluamme testata. Lopuksi käytämme expect-metodia arvioimaan testien onnistumista.

## Syvempi sukellus
Testien kirjoittaminen auttaa varmistamaan, että koodi toimii oikein ja vähentää virheiden määrää ennen koodin julkaisua tai integrointia muihin projekteihin. Se myös auttaa parantamaan koodin laatua ja helpottaa jatkokehitystä. TypeScriptin avulla voit myös käyttää muita hyödyllisiä ominaisuuksia, kuten tyyppejä ja rajapintoja, testien kirjoittamisessa.

## Katso myös
- [TypeScriptin viralliset dokumentaatiot](https://www.typescriptlang.org/docs)
- [Jest-testikehys TypeScriptille](https://jestjs.io/docs/getting-started)
- [Artikkeli testien kirjoittamisesta TypeScriptillä](https://medium.com/@allywit/typescript-and-testing-the-absolute-basics-2f32ab8c0b69)