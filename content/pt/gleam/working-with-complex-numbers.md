---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:40:23.211860-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Os números complexos possuem uma parte real e uma parte imaginária (`a + bi`). Eles são úteis em vários campos, como engenharia elétrica e computação quântica. Programadores os utilizam para modelar equações que não são solucionáveis usando apenas números reais.

## Como fazer:
Gleam não suporta nativamente números complexos. Normalmente, você criaria o seu próprio ou procuraria por uma biblioteca. Aqui está um exemplo rápido de como você poderia implementar operações básicas:

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
  let soma = add(num1, num2)
  let produto = multiply(num1, num2)

  soma // Complex(4.0, 6.0)
  produto // Complex(-5.0, 10.0)
}
```

## Aprofundamento

Os números complexos foram documentados de forma mais formal pela primeira vez por Gerolamo Cardano no século XVI. Eles são uma extensão natural dos números reais. No entanto, em uma linguagem jovem como Gleam — que prioriza performance e segurança de tipos — tais funcionalidades são básicas (ou você mesmo faz).

Em algumas outras linguagens, como Python, números complexos são incorporados (`3+4j`), facilitando a vida. Em Rust ou Haskell, você possui bibliotecas que oferecem funcionalidades avançadas prontas para uso.

A abordagem do Gleam significa que você precisa lidar com todos os aspectos: aritmética, coordenadas polares, formas exponenciais, etc. Implementar operações eficientes e precisas envolve programação cuidadosa, considerando como o comportamento de ponto flutuante pode afetar seus resultados.

Lembre-se de testar cuidadosamente, especialmente os casos limites! Lidar com infinito complexo e valores NaN (não um número) pode te confundir se você não for cuidadoso.

## Veja Também
Para mais coisas interessantes, aqui está onde você pode mergulhar:
- [Documentação Oficial do Gleam](https://gleam.run/documentation/)
- Explore as bibliotecas de outras linguagens para inspiração, como o [num-complex](https://crates.io/crates/num-complex) do Rust ou o módulo [cmath](https://docs.python.org/3/library/cmath.html) do Python.