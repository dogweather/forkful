---
title:                "Gerando números aleatórios"
html_title:           "Rust: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

A geração de números aleatórios é frequentemente utilizada em programas de jogos, simulações e criptografia. É uma maneira de introduzir elementos imprevisíveis e aleatórios em um programa, tornando-o mais dinâmico e interessante.

## Como fazer em Rust

Gerar números aleatórios em Rust é bastante simples. Primeiro, adicione a biblioteca `rand` ao seu projeto adicionando a seguinte linha ao seu arquivo `Cargo.toml`:

```Rust
[dependencies]
rand = "0.8.3"
```

Em seguida, importe a biblioteca em seu código:

```Rust
use rand::Rng;
```

Agora você pode utilizar os métodos da biblioteca `rand::Rng` para gerar números aleatórios. Por exemplo, vamos gerar um número inteiro aleatório entre 1 e 100:

```Rust
let mut rng = rand::thread_rng();
let number = rng.gen_range(1..=100); // o resultado será um número entre 1 e 100, incluindo 1 e 100
println!("O número aleatório é: {}", number);
```

Você também pode gerar números aleatórios de outros tipos, como floats e booleanos. Confira a documentação da biblioteca `rand` para obter mais informações e exemplos.

## Aprofundando-se

A geração de números aleatórios é um processo complexo e é importante entender como ela funciona. Em Rust, a biblioteca `rand` utiliza o gerador de números aleatórios Mersenne Twister para gerar valores pseudoaleatórios. Os valores gerados são chamados de "pseudoaleatórios" porque, embora sejam praticamente imprevisíveis, ainda são determinísticos, ou seja, os mesmos valores serão gerados a partir da mesma semente.

Para gerar números verdadeiramente aleatórios, é necessário utilizar uma fonte externa para gerar uma semente, como movimentos do mouse, o tempo do sistema ou hardware dedicado. A biblioteca `rand` não oferece suporte a isso, mas existem outras bibliotecas que podem ser utilizadas em conjunto com ela.

## Veja também

- Documentação da biblioteca `rand` [link](https://docs.rs/rand/0.8.3/rand/)
- Blog da Rust: "Entendendo a geração de números aleatórios em Rust" [link](https://blog.rust-lang.org/2017/01/20/rust-1.15.html#rng)
- Introdução à programação em Rust: "Geração de números aleatórios" [link](https://doc.rust-lang.org/book/ch07-07-packages-crates-and-modules.html#generating-random-numbers)