---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:12:32.762059-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen hebben een reëel deel en een imaginair deel (`a + bi`). Ze zijn handig in verschillende velden, zoals elektrotechniek en quantum computing. Programmeurs gebruiken ze om vergelijkingen te modelleren die niet oplosbaar zijn met alleen reële getallen.

## Hoe:
Gleam heeft geen native ondersteuning voor complexe getallen. Je zou normaal gesproken je eigen implementatie schrijven of een bibliotheek vinden. Hier is een snel voorbeeld van hoe je basisbewerkingen zou kunnen implementeren:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn vermenigvuldigen(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let som = add(num1, num2)
  let product = vermenigvuldigen(num1, num2)

  som // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## Diepgaande duik

Complexe getallen werden voor het eerst formeel gedocumenteerd door Gerolamo Cardano in de 16e eeuw. Ze zijn een natuurlijke uitbreiding van de reële getallen. Echter, in een jonge taal als Gleam – die prestaties en typeveiligheid prioriteert – zijn dergelijke functies basis (of je doet-het-zelf).

In sommige andere talen, zoals Python, zijn complexe getallen ingebouwd (`3+4j`), wat het leven makkelijker maakt. In Rust of Haskell heb je bibliotheken die geavanceerde functionaliteiten kant-en-klaar bieden.

De aanpak van Gleam betekent dat je alle aspecten moet aanpakken: rekenkunde, poolcoördinaten, exponentiële vormen, enz. Het implementeren van efficiënte, nauwkeurige bewerkingen vereist zorgvuldige programmering, gezien hoe het gedrag van zwevende kommagetallen je resultaten kan beïnvloeden.

Vergeet niet om grondig te testen, vooral randgevallen! Het omgaan met complexe oneindigheid en NaN (niet een nummer) waarden kan struikelblokken veroorzaken als je niet voorzichtig bent.

## Zie ook
Voor meer lekkernijen, hier is waar je kunt duiken in:
- [Gleam's Officiële Documentatie](https://gleam.run/documentation/)
- Duik in de bibliotheken van andere talen voor inspiratie, zoals Rust's [num-complex](https://crates.io/crates/num-complex) of Python's [cmath module](https://docs.python.org/3/library/cmath.html).
