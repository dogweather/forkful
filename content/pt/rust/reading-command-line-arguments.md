---
title:                "Rust: Lendo argumentos da linha de comando"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Quando se trata de criar aplicativos úteis e eficientes, é importante que os desenvolvedores tenham uma compreensão sólida sobre como lidar com dados de entrada do usuário. Uma forma comum de fazer isso é através da leitura de argumentos da linha de comando. Neste post, vamos discutir como ler esses argumentos de forma eficiente usando a linguagem de programação Rust.

## Como Fazer

Para ler os argumentos de linha de comando em Rust, podemos usar a função ```std::env::args()```, que retorna um iterador contendo todos os argumentos fornecidos pelo usuário. Vamos dar uma olhada em um exemplo simples:

```Rust
use std::env; // importa o módulo env
fn main() {
    // obtém todos os argumentos e armazena em um vetor
    let args: Vec<String> = env::args().collect();

    // verifica se há pelo menos dois argumentos
    if args.len() > 1 {
        // o primeiro argumento é o nome do programa, então começamos a contagem a partir do segundo
        for i in 1..args.len() {
            // imprime o argumento e sua posição no vetor
            println!("Argumento {}: {}", i, args[i]);
        }
    } else {
        // imprime uma mensagem de erro se o usuário não fornecer nenhum argumento
        println!("Por favor, forneça pelo menos um argumento!");
    }
}
```

Saída:

```
Argumento 1: Hello
Argumento 2: World!
```

Neste exemplo, estamos usando um for loop para iterar sobre os argumentos fornecidos pelo usuário, começando a contagem a partir do segundo argumento, já que o primeiro é sempre o nome do programa. Podemos então acessar cada argumento usando seu índice no vetor.

## Deep Dive

A função ```args()``` retorna um iterador contendo argumentos do tipo ```String```, mas isso pode ser problemático, já que alguns dos argumentos podem não estar no formato desejado. Para resolver esse problema, podemos usar a função ```env::args_os()```, que retorna um iterador contendo argumentos do tipo ```OsString```, que pode ser convertido para ```String``` se necessário. Isso garante que o nosso código seja mais robusto e possa lidar com argumentos em diferentes formatos.

## Veja Também

Para saber mais sobre a leitura de argumentos de linha de comando em Rust, confira a documentação oficial: 
- [std::env::args()](https://doc.rust-lang.org/std/env/fn.args.html)
- [std::env::args_os()](https://doc.rust-lang.org/std/env/fn.args_os.html)