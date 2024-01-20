---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Imprimir a saída de depuração em Rust significa exibir valores de variáveis e estruturas no console quando executamos código Rust. Isso permite que os programadores verifiquem o estado de seu código, facilitando a detecção e correção de erros.

## Como fazer:

Vamos mostrar alguns exemplos práticos sobre como imprimir a saída de depuração em Rust.

```Rust
fn main() {
    let x = 5;
    println!("x = {:?}", x); 
}
```
Ao executar esse código você vai ver a seguinte saída:
```Shell
x = 5
```
Neste exemplo, `{:?}` é um marcador de posição que recebe a variável `x` e a imprime na tela.

Para estruturas mais complexas, precisamos usar a derivação `Debug`:

```Rust
#[derive(Debug)]
struct Pessoa {
    nome: String,
    idade: u8,
}

fn main() {
    let pessoa = Pessoa { nome: String::from("João"), idade: 30 };
    println!("{:?}", pessoa);
}
```
A saída será semelhante a:
```Shell
Pessoa { nome: "João", idade: 30 }
```
## Mergulho profundo

A capacidade de imprimir a saída de depuração é uma característica essencial de qualquer linguagem de programação. Em Rust, é relativamente simples devido ao macro `println!`, que é uma melhoria em relação a outras linguagens que exigem importação explícita de bibliotecas ou uso de funções semelhantes para depuração.

Uma alternativa ao `println!` é o macro `dbg!`, que fornece informação adicional como a linha do código que foi executada, tornando ainda mais fácil o processo de depuração.

Em relação aos detalhes de implementação, o macro `println!` em Rust depende da implementação do trait `std::fmt::Debug`, que por sua vez usa a formatação de sequência de escape `{:?}`. Se você tentar usar `{:?}` com um tipo de dados que não implementa `Debug`, o compilador Rust irá gerar um erro em tempo de compilação.

## Ver também

Para mais informações e estudos mais aprofundados, consulte estas fontes:

1. [Documentação oficial do Rust](https://doc.rust-lang.org/std/fmt/): Um excelente recurso para entender todos os aspectos da formatação de saída e o uso de macros.
2. [The Rust Programming Language](https://nostarch.com/Rust2018): Este livro é um recurso abrangente para tudo relacionado ao Rust, inclusive a impressão de saída de depuração.
3. [The Book Chapter 5.1 - Derivação de Traits](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#adding-useful-functionality-with-derive): Aqui está uma vistas mais aprofundadas sobre como Rust maneja a derivação de traits, crucial ao se trabalhar com `Debug`.