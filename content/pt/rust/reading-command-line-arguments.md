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

## Por que ler argumentos da linha de comando?

É útil saber como ler argumentos da linha de comando quando você está desenvolvendo um programa em Rust. Isso permite que seu programa interaja com o usuário de diferentes maneiras, como receber entradas do usuário ou executar comandos específicos. Além disso, pode facilitar a execução de seu programa em diferentes plataformas.

## Como fazer

Para ler argumentos da linha de comando em Rust, você precisa importar a biblioteca padrão `std::env`. A partir disso, você pode acessar os argumentos fornecidos pelo usuário através do `std::env::args`. Cada argumento é retornado como uma string e você pode iterar sobre eles para obter o valor específico que você precisa. Aqui está um exemplo:

```Rust
use std::env;

fn main() {
    // Obtém os argumentos da linha de comando
    let args: Vec<String> = env::args().collect();
    // Itera sobre os argumentos
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

Supondo que você compile e execute o código acima com o argumento "hello" na linha de comando, o resultado será:

```
hello
```

Você também pode acessar argumentos específicos diretamente através de seu índice na lista de argumentos, assim como você faria com um vetor normal. Veja este exemplo:

```Rust
use std::env;

fn main() {
    // Obtém o segundo argumento da linha de comando
    let arg_two = env::args().nth(2);
    
    match arg_two {
        // Se o segundo argumento existir, imprime seu valor
        Some(value) => println!("{}", value),
        // Se não existir, imprime uma mensagem de erro
        None => println!("O segundo argumento não foi fornecido"),
    }
}
```

## Aprofundando um pouco mais

Existem algumas coisas importantes para se ter em mente ao ler argumentos da linha de comando em Rust. Primeiro, é importante lembrar que a lista de argumentos começa no índice 1, não no índice 0. Por exemplo, o primeiro argumento será `env::args().nth(1)`, não `env::args().nth(0)`. 
Além disso, ao converter os argumentos em um vetor de strings, o primeiro argumento (que é o nome do programa) será incluído no vetor também. Você pode usar o método `env::args().skip(1)` para ignorar o primeiro argumento, se necessário.

Outro ponto a ter em mente é que os argumentos serão retornados como strings, então você precisará convertê-los para o tipo de dado apropriado antes de usá-los em seu programa.

## Veja também

- [Documentação oficial Rust sobre leitura de argumentos da linha de comando](https://doc.rust-lang.org/stable/std/env/fn.args.html)
- [Tutorial sobre leitura de argumentos da linha de comando em Rust](https://www.tutorialspoint.com/rust-program-to-read-command-line-arguments)