---
title:                "Rust: Gerando números aleatórios"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante?

Gerar números aleatórios é uma tarefa necessária em muitos projetos de programação. Pode ser útil para criar jogos, testar algoritmos ou até mesmo para fins estatísticos. Ao aprender como gerar números aleatórios em Rust, você terá mais uma ferramenta em sua caixa de ferramentas de programação.

## Como gerar números aleatórios em Rust

Para gerar números aleatórios em Rust, usamos a biblioteca padrão `rand`. Primeiro, precisamos adicionar a dependência no nosso `Cargo.toml`:

```
[dependencias]
rand = "0.7.2"
```

Em seguida, importamos o módulo `rand` e a função `Rng` para a nossa função principal:

```
use rand::Rng;

fn main() {}
```

Para gerar um único número aleatório, podemos usar a função `gen_range()` com um intervalo específico:

```
let num = rand::thread_rng().gen_range(1..=10);
println!("Número aleatório: {}", num);
```

Este código irá gerar um número aleatório entre 1 e 10 e imprimi-lo na tela. Também podemos gerar uma sequência de números aleatórios usando um loop `for`:

```
for _ in 1..=5 {
  let num = rand::thread_rng().gen_range(1..=10);
  println!("{}", num);
}
```

Isso irá gerar 5 números aleatórios entre 1 e 10 e imprimi-los na tela.

## Aprofundando-se na geração de números aleatórios

A função `gen_range()` é apenas uma das opções para gerar números aleatórios em Rust. A biblioteca `rand` também possui outras funções úteis, como `shuffle()` para embaralhar uma lista e `choose()` para escolher um elemento aleatório de uma lista.

Algo importante a se ter em mente ao gerar números aleatórios é a semente, que é o valor inicial usado pelo gerador de números aleatórios. Se não for definida, a semente será definida como o valor atual do relógio do sistema. No entanto, se quisermos resultados previsíveis, podemos definir a semente manualmente:

```
let mut rng = rand::rngs::StdRng::seed_from_u64(42);
let num = rng.gen_range(1..=10);
```

Isso irá gerar um número aleatório usando a semente 42. Se executarmos esse código várias vezes, sempre obteremos o mesmo número.

## Veja também

- [Documentação da biblioteca rand](https://docs.rs/rand/0.7.2/rand/)
- [Vídeo tutorial da geração de números aleatórios em Rust](https://www.youtube.com/watch?v=R0DB_GcqXAc)
- [Exemplos de código para gerar números aleatórios em Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=19894ec24dd71b0c8c6bc1a992e90b5f)

Agora você está pronto para gerar números aleatórios em seus projetos Rust. Lembre-se de sempre definir uma semente, se necessário, e explorar as diferentes opções que a biblioteca `rand` oferece. Feliz programação!