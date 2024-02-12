---
title:                "Arredondamento de números"
aliases: - /pt/rust/rounding-numbers.md
date:                  2024-01-26T03:46:43.435815-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/rounding-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Arredondar números significa ajustá-los ao número inteiro mais próximo ou a uma fração com uma certa precisão. Programadores arredondam números para simplificar valores para a legibilidade humana, para atender aos requisitos de especificação, ou para reduzir a sobrecarga computacional em operações de ponto flutuante.

## Como fazer:
Rust torna o arredondamento algo fácil. Confira estes métodos para os tipos `f32` ou `f64`:

```rust
fn main() {
    let num = 2.34567;

    // Arredondar para o inteiro mais próximo
    let round = num.round();
    println!("Arredondado: {}", round); // Arredondado: 2

    // Piso - maior inteiro menor que ou igual ao número
    let floor = num.floor();
    println!("Piso: {}", floor); // Piso: 2

    // Teto - menor inteiro maior que ou igual ao número
    let ceil = num.ceil();
    println!("Teto: {}", ceil); // Teto: 3

    // Truncar - parte inteira sem dígitos fracionários
    let trunc = num.trunc();
    println!("Truncado: {}", trunc); // Truncado: 2

    // Ao múltiplo mais próximo de uma potência de dez
    let multiplo_de_dez = (num * 100.0).round() / 100.0;
    println!("Arredondado para 2 casas decimais: {}", multiplo_de_dez); // Arredondado para 2 casas decimais: 2.35
}
```

## Aprofundamento
Historicamente, o arredondamento tem sido crucial para adaptar decimais infinitos ou números irracionais em espaços digitais limitados—uma necessidade para antigos computadores com pouca memória. Pense em ábaco, mas menos artesanal, mais matemático.

Alternativas aos métodos nativos do Rust incluem:
1. Macro `format!` para formatação de string que arredonda por padrão.
2. Crates externas para tarefas matemáticas especializadas, como a crate `round` com controle mais granular.

Por baixo dos panos, as operações de arredondamento do Rust seguem os padrões IEEE—jargão técnico para "ele arredonda como o seu professor de matemática deseja". Além disso, devido às representações binárias, alguns números não podem ser arredondados tradicionalmente, como 0.1, devido à sua representação infinita em binário.

## Veja Também
- Documentação do Rust sobre métodos de tipo primitivo: https://doc.rust-lang.org/std/primitive.f64.html
- Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Crate "round" para arredondamento mais complexo: https://crates.io/crates/round
