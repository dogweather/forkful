---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:41:14.914695-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kompleksiluvuilla on reaaliosa ja imaginaariosa (`a + bi`). Ne ovat käteviä monilla aloilla, kuten sähkötekniikassa ja kvanttilaskennassa. Ohjelmoijat käyttävät niitä mallintamaan yhtälöitä, joita ei voida ratkaista käyttämällä vain reaalilukuja.

## Miten:
Gleam ei tue kompleksilukuja natiivisti. Yleensä tekisit oman toteutuksen tai etsisit kirjaston. Tässä on nopea esimerkki siitä, miten voisit toteuttaa perusoperaatioita:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## Syväsukellus

Kompleksiluvut dokumentoitiin ensimmäisen kerran tarkemmin 1500-luvulla Gerolamo Cardanon toimesta. Ne ovat luonnollinen jatkumo reaaliluvuille. Kuitenkin nuoressa kielessä, kuten Gleam, joka painottaa suorituskykyä ja tyypin turvallisuutta, tällaiset ominaisuudet ovat vähäisiä (tai tee-se-itse).

Joissain muissa kieliissä, kuten Pythonissa, kompleksiluvut ovat sisäänrakennettuja (`3+4j`), mikä tekee elämästä helpompaa. Rustissa tai Haskellissa on kirjastoja, jotka tarjoavat edistyneitä toimintoja suoraan paketista.

Gleamin lähestymistapa tarkoittaa, että sinun on käsiteltävä kaikkia aspekteja: aritmetiikkaa, napakoordinaatteja, eksponentiaalimuotoja, jne. Tehokkaiden, tarkkojen operaatioiden toteuttaminen vaatii huolellista ohjelmointia, ottaen huomioon, miten liukulukukäyttäytyminen voi vaikuttaa tuloksiisi.

Muista testata perusteellisesti, etenkin reunatapaukset! Kompleksinen äärettömyys ja NaN (ei luku) -arvojen käsittely voivat kompastaa, jos et ole varovainen.

## Katso Myös
Lisää herkkuja varten, tässä on mistä voit sukeltaa syvemmälle:
- [Gleamin viralliset dokumentit](https://gleam.run/documentation/)
- Tutustu muiden kielten kirjastoihin inspiraation lähteenä, kuten Rustin [num-complex](https://crates.io/crates/num-complex) tai Pythonin [cmath moduuli](https://docs.python.org/3/library/cmath.html).
