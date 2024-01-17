---
title:                "Lendo argumentos da linha de comando"
html_title:           "Rust: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e por que?
Ler argumentos da linha de comando é uma técnica comum na programação que permite que os programadores obtenham informações úteis a partir da linha de comando enquanto executam seu código. Isso pode ser útil para fornecer opções de configuração ou personalizar o comportamento do programa.

## Como fazer:
Para ler argumentos da linha de comando em Rust, é possível usar a função ```std::env::args()```, que retorna um iterador sobre os argumentos fornecidos. Ao usar esse iterador, é possível acessar cada argumento individualmente e utilizá-los em seu código. Veja um exemplo simples abaixo:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    // O primeiro argumento é sempre o path do arquivo executado
    println!("Meu programa está sendo executado a partir de: {}", args[0]);
    
    // Argumentos adicionais podem ser acessados a partir do index 1
    if args.len() > 1 {
        println!("O primeiro argumento fornecido é: {}", args[1]);
    }
}
```

Exemplo de uso:
```
$ ./meu_programa argumento1 argumento2
Meu programa está sendo executado a partir de: ./meu_programa
O primeiro argumento fornecido é: argumento1
```

## Mergulho profundo:
Ler argumentos da linha de comando tem raízes no Unix, onde os programas foram projetados para serem executados a partir da linha de comando. Alguns programas, como o ```ls```, possuem uma lista longa de opções e argumentos que podem ser fornecidos ao executá-los. Outra alternativa para ler argumentos da linha de comando em Rust é usar a biblioteca ```getopts```, que oferece uma abordagem mais estruturada para lidar com argumentos.

A função ```env::args()``` é implementada usando a variável de ambiente ```std::env::args_os()```, que converte os argumentos em uma forma mais genérica. Isso permite que os argumentos sejam fornecidos em UTF-8 ou outros formatos suportados pelo sistema operacional. É importante notar que a função ```env::args()``` pode retornar um erro caso não seja possível obter acesso aos argumentos da linha de comando.

## Veja também:
- [Documentação Rust sobre a função env::args()](https://doc.rust-lang.org/std/env/fn.args.html)
- [Documentação Rust sobre a biblioteca getopts](https://docs.rs/getopts/0.3.2/getopts/)