---
title:                "Å jobbe med komplekse tall"
date:                  2024-01-26T04:40:32.198842-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Komplekse tall har en reell del og en imaginær del (`a + bi`). De er nyttige i ulike felt som elektroteknikk og kvantedatamaskiner. Programmerere bruker dem til å modellere ligninger som ikke kan løses bare ved bruk av reelle tall.

## Hvordan:
Gleam har ikke innebygget støtte for komplekse tall. Vanligvis må du lage din egen eller finne et bibliotek. Her er et raskt eksempel på hvordan du kan implementere grunnleggende operasjoner:

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

## Dypdykk

Komplekse tall ble først mer formelt dokumentert av Gerolamo Cardano på 1500-tallet. De er en naturlig utvidelse av de reelle tallene. Imidlertid, i et ungt språk som Gleam—som prioriterer ytelse og type sikkerhet—er slike funksjoner grunnleggende (eller du gjør-det-selv).

I noen andre språk, som Python, er komplekse tall innebygget (`3+4j`), noe som gjør livet enklere. I Rust eller Haskell har du biblioteker som tilbyr avanserte funksjoner rett ut av boksen.

Gleams tilnærming betyr at du må håndtere alle aspekter: aritmetikk, polarkoordinater, eksponentielle former, osv. Implementering av effektive, nøyaktige operasjoner innebærer nøye programmering, med tanke på hvordan flyttalladferd kan påvirke resultatene dine.

Husk å teste grundig, spesielt grensetilfeller! Håndtering av kompleks uendelighet og NaN (ikke et tall) verdier kan snuble deg hvis du ikke er forsiktig.

## Se også
For flere godbiter, her er hvor du kan dykke inn:
- [Gleams offisielle dokumenter](https://gleam.run/documentation/)
- Dykk inn i andre språks biblioteker for inspirasjon, som Rusts [num-complex](https://crates.io/crates/num-complex) eller Pythons [cmath-modul](https://docs.python.org/3/library/cmath.html).
