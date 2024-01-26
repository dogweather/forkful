---
title:                "Att arbeta med komplexa tal"
date:                  2024-01-26T04:41:15.064253-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med komplexa tal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal har en reell del och en imaginär del (`a + bi`). De är användbara inom olika områden såsom elektroteknik och kvantdatorik. Programmerare använder dem för att modellera ekvationer som inte kan lösas med bara reella tal.

## Hur gör man:
Gleam saknar inbyggt stöd för komplexa tal. Vanligtvis måste du skapa ditt eget eller hitta ett bibliotek. Här är ett snabbt exempel på hur du kan implementera grundläggande operationer:

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

## Djupdykning

Komplexa tal dokumenterades första gången mer formellt av Gerolamo Cardano på 1500-talet. De är en naturlig utvidgning av de reella talen. Dock, i ett ungt språk som Gleam – som prioriterar prestanda och typsäkerhet – är sådana funktioner grundläggande (eller du gör det själv).

I vissa andra språk, som Python, är komplexa tal inbyggda (`3+4j`), vilket underlättar. I Rust eller Haskell har du bibliotek som erbjuder avancerade funktioner direkt ur lådan.

Gleams ansats innebär att du måste hantera alla aspekter: aritmetik, polära koordinater, exponentiella former, etc. Att implementera effektiva, exakta operationer involverar noggrann programmering med tanke på hur flyttalsbeteende kan påverka dina resultat.

Kom ihåg att testa noggrant, särskilt gränsfall! Hantering av komplex oändlighet och NaN-värden (inte ett tal) kan ställa till det om du inte är försiktig.

## Se även
För fler godsaker, här är var du kan dyka djupare:
- [Gleams officiella dokumentation](https://gleam.run/documentation/)
- Gräv i andra språks bibliotek för inspiration, som Rusts [num-complex](https://crates.io/crates/num-complex) eller Pythons [cmath-modul](https://docs.python.org/3/library/cmath.html).