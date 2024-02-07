---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:46:17.702589-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Kompleksiluvut, jotka koostuvat reaaliosasta ja imaginääriosasta (yleensä kirjoitettu muodossa a + bi), mahdollistavat laskutoimitukset, jotka ovat käytännössä tai periaatteessa mahdottomia pelkillä reaaliluvuilla. Ohjelmoijat käyttävät niitä aloilla kuten signaalinkäsittely, kvanttilaskenta ja sovellettu matematiikka, joissa kaksiulotteinen numeron esitystapa on olennainen.

## Kuinka:
Kompleksilukujen käsittely TypeScriptissä vaatii omistetun luokan. Luodaan sellainen ja käydään läpi yhteen- ja kertolasku.

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Summa: ${sum.toString()}`); // Tuloste: Summa: 4 + 6i
console.log(`Tulo: ${product.toString()}`); // Tuloste: Tulo: -5 + 10i
```

## Syväsukellus
Historiallisesti kompleksiluvut olivat kiistanalaisia - niitä kutsuttiin jopa 'kuvitteellisiksi' ilmaisemaan alkuperäistä skeptisyyttä. Nykyään ne ovat perustavanlaatuisia moderneissa matematiikassa ja tieteessä.

Vaihtoehtoja yksinkertaiselle luokallemme voisi olla olemassa olevien kirjastojen, kuten `math.js` tai `complex.js` käyttö, jotka sisältävät lisäominaisuuksia, kuten trigonometriset funktiot, eksponentiaatio ja kompleksikonjugaatio.

TypeScript-toteutuksemme yksityiskohdat tiivistyvät aritmeettisten operaatioiden määrittelyyn. `add`-metodi yksinkertaisesti lisää vastaavat osat. `multiply` soveltaa algebrassa käytettyä FOIL-menetelmää, muistaen, että `i^2 = -1`.

## Katso myös
Lisälukemista ja resursseja kompleksiluvuista ja niiden käytöstä ohjelmoinnissa:

- MDN Complex Number Algebra: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` kirjasto: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` kirjasto: https://complex-js.github.io/complex.js/
