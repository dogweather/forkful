---
title:                "Imprimindo saída de depuração"
html_title:           "Rust: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que & Por quê?

A impressão de saída de depuração é uma ferramenta essencial para os programadores durante o desenvolvimento de software. Ela permite que os desenvolvedores vejam como o programa está sendo executado e identifiquem possíveis erros ou problemas.

Os programadores utilizam a impressão de saída de depuração para testar e verificar a lógica do seu código, além de ajudar a encontrar e corrigir erros em tempo de execução.

## Como Fazer:

Vamos dar uma olhada em um exemplo simples de como imprimir saída de depuração em Rust:

```Rust
fn main() {
    let x = 10;
    println!("O valor de x é: {}", x);
}
```

Neste código, usamos o `println!()` para imprimir a mensagem "O valor de x é: 10". O `println!()` é uma macro que formata a saída para nós, com o valor da variável `x`.

## Deep Dive:

A impressão de saída de depuração tem sido uma prática comum entre os programadores há muito tempo, mas com o aumento da complexidade dos programas, surgiram ferramentas mais avançadas para ajudar na depuração.

Um exemplo é o uso de _log files_, que registram todas as saídas de depuração para uma arquivo de texto, facilitando a identificação de erros em programas maiores e mais complexos.

Outra alternativa é a utilização de debuggers, que permitem que os desenvolvedores interrompam a execução do programa em pontos específicos e vejam os valores das variáveis em tempo real.

Em Rust, a macro `println!()` é implementada como parte da biblioteca padrão (`std`), utilizando o sistema de formato do _crate_ `std::fmt`. Isso permite que os desenvolvedores personalizem a saída da impressão de depuração de acordo com suas necessidades.

## Veja Também:

- [Documentação Oficial do Rust](https://doc.rust-lang.org/std/macro.println.html)
- [Introdução ao debugging em Rust](https://medium.com/@nunoferro/introdu%C3%A7%C3%A3o-ao-debugging-no-rust-269cb6d281c1)
- [Guia de Depuração de Rust](https://rust-lang-nursery.github.io/rust-cookbook/development_tools/debugging.html)