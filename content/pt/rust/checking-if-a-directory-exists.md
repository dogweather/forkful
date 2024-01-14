---
title:                "Rust: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que checar se um diretório existe?

Ao escrever um programa em Rust, é importante garantir que tudo esteja funcionando corretamente e prevendo possíveis erros que possam ocorrer durante a execução. Uma das situações comuns é quando precisamos acessar um diretório e queremos ter certeza de que ele existe antes de executar certas operações. Neste caso, é necessário checar se o diretório existe antes de prosseguir com o código, evitando assim possíveis falhas de execução.

## Como fazer?

Para checar se um diretório existe em Rust, utilizamos a função `Path::exists()` da biblioteca padrão `std::fs`. Veja um exemplo abaixo:

```Rust
use std::fs;

fn main() {
    let directory = "my_directory";
    
    if fs::Path::new(directory).exists() {
        println!("O diretório {} existe!", directory);
    } else {
        println!("O diretório {} não existe.", directory);
    }
}
```

Neste exemplo, criamos uma variável `directory` com o nome do diretório que desejamos checar. Em seguida, utilizamos a função `Path::exists()` para verificar se esse diretório existe. Caso sim, imprimiremos uma mensagem indicando sua existência. Caso contrário, imprimimos uma mensagem informando que o diretório não existe.

## Aprofundando um pouco mais

Além de checar a existência de um diretório, podemos também checar a existência de um arquivo utilizando a mesma função `Path::exists()`. Além disso, podemos usar a função `Path::is_dir()` para verificar se um caminho se refere a um diretório ou a um arquivo.

Outra forma de checar se um diretório existe é utilizando a macro `std::fs::DirEntry` do módulo `std::fs::ReadDir`. Essa macro possui uma função `DirEntry::is_dir()` que retorna `true` se o caminho especificado for um diretório.

Lembre-se de tratar erros ao lidar com arquivos e diretórios, utilizando o resultado da função `exists()` e tratando possíveis erros que possam ocorrer.

## Veja também

- Documentação oficial: https://doc.rust-lang.org/std/fs/fn.exists.html
- Artigo sobre manipulação de arquivos em Rust: https://blog.octos.com.br/manipulacao-de-arquivos-como-trabalhar-com-io-em-rust/ 
- Projeto no GitHub/Aprendendo Rust: https://github.com/aprendendo-rust/fs-example