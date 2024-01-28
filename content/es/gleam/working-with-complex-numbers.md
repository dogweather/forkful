---
title:                "Trabajando con números complejos"
date:                  2024-01-26T04:40:24.977723-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con números complejos"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Los números complejos tienen una parte real y una parte imaginaria (`a + bi`). Son útiles en varios campos como la ingeniería eléctrica y la computación cuántica. Los programadores los usan para modelar ecuaciones que no se pueden resolver usando solo números reales.

## Cómo hacerlo:
Gleam carece de soporte nativo para números complejos. Normalmente, tendrías que crear tu propia implementación o encontrar una biblioteca. Aquí hay un ejemplo rápido de cómo podrías implementar operaciones básicas:

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

## Análisis profundo

Los números complejos fueron documentados de manera más formal por primera vez por Gerolamo Cardano en el siglo XVI. Son una extensión natural de los números reales. Sin embargo, en un lenguaje joven como Gleam, que prioriza el rendimiento y la seguridad de tipo, dichas características son básicas (o lo haces tú mismo).

En algunos otros idiomas, como Python, los números complejos están incorporados (`3+4j`), lo que facilita la vida. En Rust o Haskell, tienes bibliotecas que ofrecen funcionalidades avanzadas de manera inmediata.

El enfoque de Gleam significa que tienes que manejar todos los aspectos: aritmética, coordenadas polares, formas exponenciales, etc. Implementar operaciones eficientes y precisas implica una programación cuidadosa, considerando cómo el comportamiento de los números de punto flotante puede afectar tus resultados.

Recuerda probar a fondo, especialmente los casos límite. Manejar infinito complejo y valores NaN (no es un número) puede complicarte si no eres cuidadoso.

## Ver también
Para más contenidos, aquí es donde puedes profundizar:
- [Documentación oficial de Gleam](https://gleam.run/documentation/)
- Explora las bibliotecas de otros lenguajes para inspirarte, como el [num-complex](https://crates.io/crates/num-complex) de Rust o el [módulo cmath](https://docs.python.org/3/library/cmath.html) de Python.
