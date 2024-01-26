---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:51.300270-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Gerar números aleatórios é bolar números que não seguem um padrão previsível. Programadores fazem isso por várias razões, tipo jogos, simulações e segurança da informação, onde o acaso é essencial.

## Como Fazer:
Para gerar números aleatórios em Rust, você pode usar a crate `rand`. Aqui está um exemplo básico de como usá-la:

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let numero_aleatorio: i32 = rng.gen();
    println!("Número aleatório: {}", numero_aleatorio);
}
```

Esse código vai imprimir algo como:

```
Número aleatório: 684317153
```

## Aprofundando:
Gerar números realmente aleatórios é um baita desafio. Originalmente, os números aleatórios vinham de fontes físicas, tipo rodadas de dados ou ruídos atmosféricos. Online é mais complicado. A crate `rand` do Rust oferece vários métodos de geração de aleatoriedade, desde simples e rápidos até os criptograficamente seguros.

Alternativas incluem o uso do, `rand::thread_rng()` para uso geral ou `rand::random()` para um atalho rápido, mas menos flexível. Você também pode usar `rand::OsRng` para números que atendem aos critérios de segurança.

A implementação por trás das cortinas se apoia em algoritmos como Mersenne Twister ou ChaCha, dependendo do nível de segurança e performance necessários. Ademais, o Rust se assegura que os números aleatórios são gerados de maneira eficaz em termos de recursos e seguros para operações concorrentes.

## Veja Também:
- Documentação oficial da crate `rand`: https://crates.io/crates/rand
- Um tutorial mais a fundo sobre a geração de números aleatórios: https://rust-lang-nursery.github.io/rust-cookbook/algorithms/randomness.html
- Capítulo do Rust Book sobre geradores de números aleatórios: https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html#generating-a-secret-number
