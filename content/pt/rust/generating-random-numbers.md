---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Gerar números aleatórios é o processo de produção de números que não seguem um padrão previsível. Programadores os usam para criar dinamismo em jogos, simulação de variabilidade em testes automatizados e muitas outras aplicações importantes.

## Como Fazer:

Para começar, adicione a dependência `rand` no arquivo `Cargo.toml`:

```Rust
[dependencies]
rand = "0.8.4"
```

Agora podemos usar a função `rng.gen_range()` para gerar um número aleatório:

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let n: u32 = rng.gen_range(0..100);
    println!("Número aleatório: {}", n);
}
```

Quando você rodar este código, deve ver a seguinte saída (o número vai variar):

```Rust
Número aleatório: 42
```

## Aprofundando-se

Historicamente, a geração de números aleatórios na programação foi um desafio devido à sua natureza determinística. No entanto, Rust aborda isso com o pacote `rand`, que cria números pseudo-aleatórios, muito difíceis de prever.

Embora o `rand` seja a maneira mais comum de gerar números aleatórios em Rust, existem alternativas. Libs como `fastrand` e `randomize` oferecem alternativas confiáveis.

O `rand` utiliza um gerador de números pseudoaleatórios sob o capô. Como esses números são determinados por um algoritmo, eles não são verdadeiramente "aleatórios". No entanto, sem acesso ao algoritmo ou à semente inicial, seria quase impossível prever a sequência de números.

## Ver Também

1. Documentação da Crates.io para `rand`: [link](https://crates.io/crates/rand)
2. Livro do Rust sobre geradores de números aleatórios: [link](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)
3. Documentação mais aprofundada sobre `rand::Rng`: [link](https://doc.rust-lang.org/rand/rand/trait.Rng.html)