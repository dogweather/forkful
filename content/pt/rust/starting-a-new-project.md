---
title:    "Rust: Começando um novo projeto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que iniciar um novo projeto com Rust?

Se você é um programador que está sempre em busca de ferramentas poderosas e eficientes, então o Rust é uma linguagem que definitivamente deve estar no seu radar. Com sua sintaxe limpa e moderna, Rust oferece segurança de memória e concorrência sem comprometer o desempenho. Se você está procurando iniciar um novo projeto, aqui estão algumas razões pelas quais você deve considerar o Rust.

## Como começar um novo projeto com Rust?

A primeira coisa que você precisa fazer é instalar o compilador de Rust e suas ferramentas relacionadas. Você pode fazer isso seguindo as instruções detalhadas em seu site oficial. Depois de instalar, você pode iniciar um novo projeto usando o gerenciador de pacotes Cargo. Basta digitar o seguinte comando em seu terminal:

```Rust
cargo new nome_do_projeto
```

Este comando irá criar uma nova pasta com o nome do seu projeto e gerar os arquivos básicos necessários. Agora você está pronto para começar a codificar em Rust!

Um dos principais recursos do Rust é o sistema de propriedade, que garante que seu código seja seguro e sem erros. Isso é especialmente útil para projetos grandes e complexos, pois ajuda a evitar muitos bugs comuns. Aqui está um exemplo de código que utiliza o sistema de propriedade para garantir a segurança de memória:

```Rust
fn main() {
  let mut vetor = vec![1, 2, 3];

  for numero in &vetor {
    println!("{}", numero);
  }

  vetor.push(4); // Isso irá gerar um erro de compilação, pois estamos tentando modificar um vetor imutável
}
```

Com o Rust, você também pode escrever código concorrente sem se preocupar com condições de corrida. Aqui está um exemplo de um programa que usa threads para imprimir números de 0 a 9, em ordem aleatória:

```Rust
use std::thread;
use std::time::Duration;
use rand::Rng;

fn main() {
  let mut threads = vec![];

  for i in 0..10 {
    threads.push(thread::spawn(move || {
      thread::sleep(Duration::from_secs(rand::thread_rng().gen_range(1..10)));
      println!("{}", i);
    }));
  }

  for thread in threads {
    thread.join().unwrap();
  }
}
```

## Aprofundando-se em um novo projeto com Rust

Rust é uma linguagem relativamente nova, mas está crescendo rapidamente em popularidade. Existem muitos recursos disponíveis on-line onde você pode aprender mais sobre a linguagem e como usá-la em seus projetos. Além disso, a comunidade de Rust é extremamente ativa e acolhedora, então você sempre pode pedir ajuda e aprender com outros programadores.

Outro aspecto importante a ter em mente é que, como o Rust é uma linguagem de baixo nível, pode ser um pouco mais difícil de aprender do que linguagens de alto nível como Python ou JavaScript. No entanto, os benefícios de desempenho e segurança de memória que ele oferece fazem valer a pena investir tempo para se familiarizar com ele.

## Veja também

- [Site oficial do Rust](https://www.rust-lang.org/pt-BR/)
- [Documentação do Cargo](https://doc.rust-lang.org/cargo/)
- [Comunidade Rust Brasil](https://www.rust-br.org/)